using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Runtime.Serialization;
using System.Security.Permissions;

namespace Microcode
{
    [Serializable]
    class PrimitiveCombination3 : SCode, ISerializable, ISystemVector
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public override TC TypeCode { get { return TC.PCOMB3; } }

#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand2TypeHistogram = new Histogram<Type> ();
        static Histogram<string> histogram = new Histogram<String> ();
        [NonSerialized]
        protected string histogramKey;

        [NonSerialized]
        protected Type rand0Type;

        [NonSerialized]
        protected Type rand1Type;

        [NonSerialized]
        protected Type rand2Type;
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly Primitive3 procedure;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        [NonSerialized]
        protected PrimitiveMethod3 method;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode arg0;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode arg1;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode arg2;

        protected PrimitiveCombination3 (Primitive3 procedure, SCode arg0, SCode arg1, SCode arg2)
            : base ()
        {
            if (procedure == null) throw new ArgumentNullException ("procedure");
            this.procedure = procedure;
            this.method = procedure.Method;
            this.arg0 = arg0;
            this.arg1 = arg1;
            this.arg2 = arg2;
#if DEBUG
            this.histogramKey = procedure.ToString () + " " + this.arg0.GetType ().Name.ToString () + " " + this.arg1.GetType ().Name.ToString () + " " + this.arg2.GetType ().Name.ToString ();
            rand0Type = this.arg0.GetType ();
            rand1Type = this.arg1.GetType ();
            rand2Type = this.arg2.GetType ();
#endif
        }

        public static SCode Make (Primitive3 procedure, SCode arg0, SCode arg1, SCode arg2)
        {
            return
                (!Configuration.EnablePrimitiveCombination3Optimization) ? new PrimitiveCombination3 (procedure, arg0, arg1, arg2) :
                (arg0 is Argument) ? PrimitiveCombination3A.Make (procedure, (Argument) arg0, arg1, arg2) :
                (arg0 is Quotation) ? PrimitiveCombination3Q.Make (procedure, (Quotation) arg0, arg1, arg2) :
                (arg0 is StaticVariable) ? PrimitiveCombination3S.Make (procedure, (StaticVariable) arg0, arg1, arg2) :
                (arg1 is Argument) ? PrimitiveCombination3XA.Make (procedure, arg0, (Argument) arg1, arg2) :
                (arg1 is Quotation) ? PrimitiveCombination3XQ.Make (procedure, arg0, (Quotation) arg1, arg2) :
                (arg1 is StaticVariable) ? PrimitiveCombination3XS.Make (procedure, arg0, (StaticVariable) arg1, arg2) :
                (arg2 is Argument) ? PrimitiveCombination3XXA.Make (procedure, arg0, arg1, (Argument) arg2) :
                (arg2 is StaticVariable) ? new PrimitiveCombination3XXS (procedure, arg0, arg1, (StaticVariable) arg2) :
                (arg2 is Quotation) ? new PrimitiveCombination3XXQ (procedure, arg0, arg1, (Quotation) arg2) :
                //(procedure == Primitive.Hunk3Cons) ? PrimitiveHunk3Cons.Make (arg0, arg1, arg2) :
                //(procedure == Primitive.RecordSet) ? PrimitiveRecordSet.Make (arg0, arg1, arg2) :
                //(procedure == Primitive.StringSet) ? PrimitiveStringSet.Make (arg0, arg1, arg2) :
                //(procedure == Primitive.SystemPairCons) ? PrimitiveSystemPairCons.Make (arg0, arg1, arg2) :
                //(procedure == Primitive.Vector8BSet) ? PrimitiveVector8BSet.Make (arg0, arg1, arg2) :
                //(procedure == Primitive.VectorSet) ? PrimitiveVectorSet.Make (arg0, arg1, arg2) :
                new PrimitiveCombination3 (procedure, arg0, arg1, arg2);
        }

        public static SCode Make (object procedure, object arg0, object arg1, object arg2)
        {
            return
                PrimitiveCombination3.Make ((Primitive3) procedure, EnsureSCode (arg0), EnsureSCode (arg1), EnsureSCode (arg2));
        }

        public static SCode Make (Cons elements)
        {
            object proc = elements.Car;
            Cons tail0 = (Cons) elements.Cdr;
            object arg0 = tail0.Car;
            Cons tail1 = (Cons) tail0.Cdr;
            object arg1 = tail1.Car;
            Cons tail2 = (Cons) tail1.Cdr;
            object arg2 = tail2.Car;
            object tail3 = tail2.Cdr;
            if (tail3 == null)
                return Make (proc, arg0, arg1, arg2);
            else
                throw new NotImplementedException ();
        }

        [OnDeserialized ()]
        internal void OnDeserializedMethod (StreamingContext context)
        {
            this.method = procedure.Method;
        }

        public Primitive3 Operator
        {
            [DebuggerStepThrough]
            get
            {
                return this.procedure;
            }
        }

        public SCode Operand0
        {
            [DebuggerStepThrough]
            get
            {
                return this.arg0;
            }
        }

        public SCode Operand1
        {
            [DebuggerStepThrough]
            get
            {
                return this.arg1;
            }
        }

        public SCode Operand2
        {
            [DebuggerStepThrough]
            get
            {
                return this.arg2;
            }
        }

        [SchemePrimitive ("PRIMITIVE-COMBINATION3?", 1, true)]
        public static bool IsPrimitiveCombination3 (out object answer, object arg)
        {
            answer = arg is PrimitiveCombination3;
            return false;
        }

        public override bool CallsTheEnvironment ()
        {
            return this.arg0.CallsTheEnvironment ()
                || this.arg1.CallsTheEnvironment ()
                || this.arg2.CallsTheEnvironment ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            NoteCalls (this.arg1);
            NoteCalls (this.arg2);
            histogram.Note (this.histogramKey);
            ratorHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
            rand2TypeHistogram.Note (this.rand2Type);
            SCode.location = "PrimitiveCombination3";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3";
#endif
            if (ev2 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object ev1;
            env = environment;
            unev = this.arg1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame1 (this, environment, ev2));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object ev0;
            env = environment;
            unev = this.arg0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };

#if DEBUG
            SCode.location = "PrimitiveCombination3";
#endif
            if (ev0 == Interpreter.UnwindStack) throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, ev1, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }

        public override bool MutatesAny (Symbol [] formals)
        {
            return this.arg0.MutatesAny (formals)
                || this.arg1.MutatesAny (formals)
                || this.arg2.MutatesAny (formals);
        }

        #region ISystemVector Members

        public int SystemVectorSize
        {
            get { return 4; }
        }

        public object SystemVectorRef (int index)
        {
            switch (index) {
                case 0: return this.Operator;
                case 1: return this.Operand0;
                case 2: return this.Operand1;
                case 3: return this.Operand2;
                default:
                    throw new NotImplementedException ();
            }
        }

        public object SystemVectorSet (int index, object newValue)
        {
            throw new NotImplementedException ();
        }

        #endregion

        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        void ISerializable.GetObjectData (SerializationInfo info, StreamingContext context)
        {
            info.SetType (typeof (PrimitiveCombination3Deserializer));
            info.AddValue ("procedure", this.procedure);
            info.AddValue ("rand0", this.arg0);
            info.AddValue ("rand1", this.arg1);
            info.AddValue ("rand2", this.arg2);
        }

        internal override PartialResult PartialEval (PartialEnvironment environment)
        {
            PartialResult r0 = this.arg0.PartialEval (environment);
            PartialResult r1 = this.arg1.PartialEval (environment);
            PartialResult r2 = this.arg2.PartialEval (environment);
            return new PartialResult (
                r0.Residual == this.arg0 &&
                r1.Residual == this.arg1 &&
                r2.Residual == this.arg2 ? this : PrimitiveCombination3.Make (this.procedure, r0.Residual, r1.Residual, r2.Residual));
        }

        public override void CollectFreeVariables (HashSet<Symbol> freeVariableSet)
        {
            this.arg0.CollectFreeVariables (freeVariableSet);
            this.arg1.CollectFreeVariables (freeVariableSet);
            this.arg2.CollectFreeVariables (freeVariableSet);
        }
    }

    [Serializable]
    internal sealed class PrimitiveCombination3Deserializer : IObjectReference
    {
        Primitive3 procedure;
        SCode rand0;
        SCode rand1;
        SCode rand2;

        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        public Object GetRealObject (StreamingContext context)
        {
            return PrimitiveCombination3.Make (this.procedure, this.rand0, this.rand1, this.rand2);
        }

        Primitive3 Procedure { set { this.procedure = value; } }
        SCode Rand0 { set { this.rand0 = value; } }
        SCode Rand1 { set { this.rand1 = value; } }
        SCode Rand2 { set { this.rand2 = value; } }
    }

    sealed class PrimitiveCombination3Frame0 : SubproblemContinuation<PrimitiveCombination3>, ISystemVector
    {
        public PrimitiveCombination3Frame0 (PrimitiveCombination3 expression, Environment environment)
            : base (expression, environment)
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

        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object ev2)
        {
            object ev1;
            Control unev = this.expression.Operand1;
            Environment env = this.environment;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            unev = this.expression.Operand0;
            env = this.environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.expression.Operator);
            SCode.location = this.expression.Operator.Name.ToString ();
#endif
            if (this.expression.Operator.Method (out answer, ev0, ev1, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    sealed class PrimitiveCombination3Frame1 : SubproblemContinuation<PrimitiveCombination3>, ISystemVector
    {
        object ev2;
        public PrimitiveCombination3Frame1 (PrimitiveCombination3 expression, Environment environment, object ev2)
            : base (expression, environment)
        {
            this.ev2 = ev2;
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

        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object ev1)
        {
            Control unev = this.expression.Operand0;
            Environment env = this.environment;
            object ev0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            Primitive.hotPrimitives.Note (this.expression.Operator);
            SCode.location = this.expression.Operator.Name.ToString ();
#endif
            if (this.expression.Operator.Method (out answer, ev0, ev1, this.ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    answer = null;
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

    [Serializable]
    class PrimitiveCombination3A : PrimitiveCombination3
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand2TypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand0Offset;

        protected PrimitiveCombination3A (Primitive3 procedure, Argument arg0, SCode arg1, SCode arg2)
            : base (procedure, arg0, arg1, arg2)
        {
            this.rand0Offset = arg0.Offset;
        }

        public static SCode Make (Primitive3 procedure, Argument arg0, SCode arg1, SCode arg2)
        {
            return
                (arg0 is Argument0) ? PrimitiveCombination3A0.Make (procedure, (Argument0) arg0, arg1, arg2) :
                (arg0 is Argument1) ? PrimitiveCombination3A1.Make (procedure, (Argument1) arg0, arg1, arg2) :
                (arg1 is Quotation) ? PrimitiveCombination3AQ.Make (procedure, arg0, (Quotation) arg1, arg2) :
                new PrimitiveCombination3A (procedure, arg0, arg1, arg2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg1);
            NoteCalls (this.arg2);
            ratorHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            rand2TypeHistogram.Note (this.rand2Type);
            SCode.location = "PrimitiveCombination3A";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3A";
#endif
            if (ev2 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object ev1;
            env = environment;
            unev = this.arg1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3A";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame1 (this, environment, ev2));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, environment.ArgumentValue(this.rand0Offset), ev1, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class PrimitiveCombination3A0 : PrimitiveCombination3A
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand2TypeHistogram = new Histogram<Type> ();
#endif

        protected PrimitiveCombination3A0 (Primitive3 procedure, Argument0 arg0, SCode arg1, SCode arg2)
            : base (procedure, arg0, arg1, arg2)
        {
        }

        public static SCode Make (Primitive3 procedure, Argument0 arg0, SCode arg1, SCode arg2)
        {
            return
                (arg1 is Argument) ? PrimitiveCombination3A0A.Make (procedure, arg0, (Argument) arg1, arg2) :
                (arg1 is Quotation) ? PrimitiveCombination3A0Q.Make (procedure, arg0, (Quotation) arg1, arg2) :
                (arg1 is StaticVariable) ? PrimitiveCombination3A0S.Make (procedure, arg0, (StaticVariable) arg1, arg2) :
                (arg2 is Argument) ? PrimitiveCombination3A0XA.Make (procedure, arg0, arg1, (Argument) arg2) :
                (arg2 is Quotation) ? new PrimitiveCombination3A0XQ (procedure, arg0, arg1, (Quotation) arg2) :
                new PrimitiveCombination3A0 (procedure, arg0, arg1, arg2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg1);
            NoteCalls (this.arg2);
            ratorHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            rand2TypeHistogram.Note (this.rand2Type);
            SCode.location = "PrimitiveCombination3A0";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3A0";
#endif
            if (ev2 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object ev1;
            env = environment;
            unev = this.arg1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3A0";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame1 (this, environment, ev2));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, environment.Argument0Value, ev1, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class PrimitiveCombination3A0A : PrimitiveCombination3A0
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand2TypeHistogram = new Histogram<Type> ();
#endif

        public readonly int rand1Offset;
        protected PrimitiveCombination3A0A (Primitive3 procedure, Argument0 arg0, Argument arg1, SCode arg2)
            : base (procedure, arg0, arg1, arg2)
        {
            this.rand1Offset = arg1.Offset;
        }

        public static SCode Make (Primitive3 procedure, Argument0 arg0, Argument arg1, SCode arg2)
        {
            return
                (arg1 is Argument0) ? PrimitiveCombination3A0A0.Make (procedure, arg0, (Argument0) arg1, arg2) :
                (arg1 is Argument1) ? PrimitiveCombination3A0A1.Make (procedure, arg0, (Argument1) arg1, arg2) :
               new PrimitiveCombination3A0A (procedure, arg0, arg1, arg2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg2);
            ratorHistogram.Note (this.procedure);
            rand2TypeHistogram.Note (this.rand2Type);
            SCode.location = "PrimitiveCombination3A0A";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3A0A";
#endif
            if (ev2 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, environment.Argument0Value, environment.ArgumentValue (this.rand1Offset), ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class PrimitiveCombination3A0A0 : PrimitiveCombination3A0A
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand2TypeHistogram = new Histogram<Type> ();
#endif

        protected PrimitiveCombination3A0A0 (Primitive3 procedure, Argument0 arg0, Argument0 arg1, SCode arg2)
            : base (procedure, arg0, arg1, arg2)
        {
        }

        public static SCode Make (Primitive3 procedure, Argument0 arg0, Argument0 arg1, SCode arg2)
        {
            return
               new PrimitiveCombination3A0A0 (procedure, arg0, arg1, arg2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg2);
            ratorHistogram.Note (this.procedure);
            rand2TypeHistogram.Note (this.rand2Type);
            SCode.location = "PrimitiveCombination3A0A0";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3A0A0";
#endif
            if (ev2 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument0Value, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class PrimitiveCombination3A0A1 : PrimitiveCombination3A0A
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand2TypeHistogram = new Histogram<Type> ();
#endif

        protected PrimitiveCombination3A0A1 (Primitive3 procedure, Argument0 arg0, Argument1 arg1, SCode arg2)
            : base (procedure, arg0, arg1, arg2)
        {
        }

        public static SCode Make (Primitive3 procedure, Argument0 arg0, Argument1 arg1, SCode arg2)
        {
            return
                (arg2 is Argument) ? PrimitiveCombination3A0A1A.Make (procedure, arg0, arg1, (Argument) arg2) :
               new PrimitiveCombination3A0A1 (procedure, arg0, arg1, arg2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg2);
            ratorHistogram.Note (this.procedure);
            rand2TypeHistogram.Note (this.rand2Type);
            SCode.location = "PrimitiveCombination3A0A1";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3A0A1";
#endif
            if (ev2 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class PrimitiveCombination3A0A1A : PrimitiveCombination3A0A1
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
#endif
        public readonly int rand2Offset;
        protected PrimitiveCombination3A0A1A (Primitive3 procedure, Argument0 arg0, Argument1 arg1, Argument arg2)
            : base (procedure, arg0, arg1, arg2)
        {
        this.rand2Offset = arg2.Offset;
        }

        public static SCode Make (Primitive3 procedure, Argument0 arg0, Argument1 arg1, Argument arg2)
        {
            return
                (arg2 is Argument0) ? new PrimitiveCombination3A0A1A0 (procedure, arg0, arg1, (Argument0) arg2) :
                (arg2 is Argument1) ? new PrimitiveCombination3A0A1A1 (procedure, arg0, arg1, (Argument1) arg2) :
               new PrimitiveCombination3A0A1A (procedure, arg0, arg1, arg2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.procedure);
            SCode.location = "PrimitiveCombination3A0A1A";
#endif

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value, environment.ArgumentValue(this.rand2Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3A0A1A0 : PrimitiveCombination3A0A1A
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
#endif
        internal PrimitiveCombination3A0A1A0 (Primitive3 procedure, Argument0 arg0, Argument1 arg1, Argument0 arg2)
            : base (procedure, arg0, arg1, arg2)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.procedure);
            SCode.location = "PrimitiveCombination3A0A1A0";
#endif

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3A0A1A1 : PrimitiveCombination3A0A1A
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
#endif
        internal PrimitiveCombination3A0A1A1 (Primitive3 procedure, Argument0 arg0, Argument1 arg1, Argument1 arg2)
            : base (procedure, arg0, arg1, arg2)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.procedure);
            SCode.location = "PrimitiveCombination3A0A1A1";
#endif

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, environment.Argument0Value, environment.Argument1Value, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class PrimitiveCombination3A0Q : PrimitiveCombination3A0
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand2TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Value;
        protected PrimitiveCombination3A0Q (Primitive3 procedure, Argument0 arg0, Quotation arg1, SCode arg2)
            : base (procedure, arg0, arg1, arg2)
        {
            this.rand1Value = arg1.Quoted;
        }

        public static SCode Make (Primitive3 procedure, Argument0 arg0, Quotation arg1, SCode arg2)
        {
            return
                (arg2 is Argument) ? PrimitiveCombination3A0QA.Make (procedure, arg0, arg1, (Argument) arg2) :
                (arg2 is Quotation) ? new PrimitiveCombination3A0QQ (procedure, arg0, arg1, (Quotation) arg2) :
                (arg2 is StaticVariable) ? new PrimitiveCombination3A0QS (procedure, arg0, arg1, (StaticVariable) arg2) :
                new PrimitiveCombination3A0Q (procedure, arg0, arg1, arg2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg2);
            ratorHistogram.Note (this.procedure);
            rand2TypeHistogram.Note (this.rand2Type);
            SCode.location = "PrimitiveCombination3A0Q";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3A0Q";
#endif
            if (ev2 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class PrimitiveCombination3A0QA : PrimitiveCombination3A0Q
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
#endif
        public readonly int rand2Offset;
        protected PrimitiveCombination3A0QA (Primitive3 procedure, Argument0 arg0, Quotation arg1, Argument arg2)
            : base (procedure, arg0, arg1, arg2)
        {
            this.rand2Offset = arg2.Offset;
        }

        public static SCode Make (Primitive3 procedure, Argument0 arg0, Quotation arg1, Argument arg2)
        {
            return
                (arg2 is Argument0) ? new PrimitiveCombination3A0QA0 (procedure, arg0, arg1, (Argument0) arg2) :
                (arg2 is Argument1) ? new PrimitiveCombination3A0QA1 (procedure, arg0, arg1, (Argument1) arg2) :
                new PrimitiveCombination3A0QA (procedure, arg0, arg1, arg2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.procedure);
            SCode.location = "PrimitiveCombination3A0QA";
#endif

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value, environment.ArgumentValue (this.rand2Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3A0QA0 : PrimitiveCombination3A0QA
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
#endif
        internal PrimitiveCombination3A0QA0 (Primitive3 procedure, Argument0 arg0, Quotation arg1, Argument0 arg2)
            : base (procedure, arg0, arg1, arg2)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.procedure);
            SCode.location = "PrimitiveCombination3A0QA0";
#endif

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3A0QA1 : PrimitiveCombination3A0QA
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
#endif
        internal PrimitiveCombination3A0QA1 (Primitive3 procedure, Argument0 arg0, Quotation arg1, Argument1 arg2)
            : base (procedure, arg0, arg1, arg2)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.procedure);
            SCode.location = "PrimitiveCombination3A0QA0";
#endif

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3A0QQ : PrimitiveCombination3A0Q
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
#endif

        public readonly object rand2Value;
        internal PrimitiveCombination3A0QQ (Primitive3 procedure, Argument0 arg0, Quotation arg1, Quotation arg2)
            : base (procedure, arg0, arg1, arg2)
        {
            this.rand2Value = arg2.Quoted;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.procedure);
            SCode.location = "PrimitiveCombination3A0QQ";
#endif

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value, this.rand2Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3A0QS : PrimitiveCombination3A0Q
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
#endif

        public readonly Symbol rand2Name;
        public readonly int rand2Offset;
        internal PrimitiveCombination3A0QS (Primitive3 procedure, Argument0 arg0, Quotation arg1, StaticVariable arg2)
            : base (procedure, arg0, arg1, arg2)
        {
            this.rand2Name = arg2.Name;
            this.rand2Offset = arg2.Offset;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.procedure);
            SCode.location = "PrimitiveCombination3A0QS";
#endif
            object ev2;
            if (environment.StaticValue (out ev2, this.rand2Name, this.rand2Offset))
                throw new NotImplementedException ();
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, environment.Argument0Value, this.rand1Value, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class PrimitiveCombination3A0S : PrimitiveCombination3A0
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand2TypeHistogram = new Histogram<Type> ();
#endif

        public readonly Symbol rand1Name;
        public readonly int rand1Offset;
        protected PrimitiveCombination3A0S (Primitive3 procedure, Argument0 arg0, StaticVariable arg1, SCode arg2)
            : base (procedure, arg0, arg1, arg2)
        {
            this.rand1Name = arg1.Name;
            this.rand1Offset = arg1.Offset;
        }

        public static SCode Make (Primitive3 procedure, Argument0 arg0, StaticVariable arg1, SCode arg2)
        {
            return
                (arg2 is Argument) ? PrimitiveCombination3A0SA.Make (procedure, arg0, arg1, (Argument) arg2) :
               new PrimitiveCombination3A0S (procedure, arg0, arg1, arg2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg2);
            ratorHistogram.Note (this.procedure);
            rand2TypeHistogram.Note (this.rand2Type);
            SCode.location = "PrimitiveCombination3A0S";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3A0S";
#endif
            if (ev2 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }
            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, environment.Argument0Value, ev1, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class PrimitiveCombination3A0SA : PrimitiveCombination3A0S
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
#endif

        public readonly int rand2Offset;
        protected PrimitiveCombination3A0SA (Primitive3 procedure, Argument0 arg0, StaticVariable arg1, Argument arg2)
            : base (procedure, arg0, arg1, arg2)
        {
            this.rand2Offset = arg2.Offset;
        }

        public static SCode Make (Primitive3 procedure, Argument0 arg0, StaticVariable arg1, Argument arg2)
        {
            return
                (arg2 is Argument0) ? new PrimitiveCombination3A0SA0 (procedure, arg0, arg1, (Argument0) arg2) :
                (arg2 is Argument1) ? new PrimitiveCombination3A0SA1 (procedure, arg0, arg1, (Argument1) arg2) :
               new PrimitiveCombination3A0SA (procedure, arg0, arg1, arg2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.procedure);
            SCode.location = "PrimitiveCombination3A0SA";
#endif

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, environment.Argument0Value, ev1, environment.ArgumentValue(this.rand2Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3A0SA0 : PrimitiveCombination3A0SA
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
#endif

        internal PrimitiveCombination3A0SA0 (Primitive3 procedure, Argument0 arg0, StaticVariable arg1, Argument0 arg2)
            : base (procedure, arg0, arg1, arg2)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.procedure);
            SCode.location = "PrimitiveCombination3A0SA0";
#endif

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, environment.Argument0Value, ev1, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3A0SA1 : PrimitiveCombination3A0SA
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
#endif

        internal PrimitiveCombination3A0SA1 (Primitive3 procedure, Argument0 arg0, StaticVariable arg1, Argument1 arg2)
            : base (procedure, arg0, arg1, arg2)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.procedure);
            SCode.location = "PrimitiveCombination3A0SA1";
#endif

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, environment.Argument0Value, ev1, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class PrimitiveCombination3A0XA : PrimitiveCombination3A0
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand2Offset;

        protected PrimitiveCombination3A0XA (Primitive3 procedure, Argument0 arg0, SCode arg1, Argument arg2)
            : base (procedure, arg0, arg1, arg2)
        {
            this.rand2Offset = arg2.Offset;
        }

        public static SCode Make (Primitive3 procedure, Argument0 arg0, SCode arg1, Argument arg2)
        {
            return
                (arg2 is Argument0) ? new PrimitiveCombination3A0XA0 (procedure, arg0, arg1, (Argument0) arg2) :
                (arg2 is Argument1) ? new PrimitiveCombination3A0XA1 (procedure, arg0, arg1, (Argument1) arg2) :
                new PrimitiveCombination3A0XA (procedure, arg0, arg1, arg2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg1);
            ratorHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveCombination3A0XA";
#endif
            object ev2 = environment.ArgumentValue(this.rand2Offset);

            object ev1;
            Environment env = environment;
            Control unev = this.arg1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3A0XA";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame1 (this, environment, ev2));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, environment.Argument0Value, ev1, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3A0XA0 : PrimitiveCombination3A0XA
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif

        internal PrimitiveCombination3A0XA0 (Primitive3 procedure, Argument0 arg0, SCode arg1, Argument0 arg2)
            : base (procedure, arg0, arg1, arg2)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg1);
            ratorHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveCombination3A0XA0";
#endif
            object ev2 = environment.Argument0Value;

            object ev1;
            Environment env = environment;
            Control unev = this.arg1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3A0XA0";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame1 (this, environment, ev2));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, environment.Argument0Value, ev1, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3A0XA1 : PrimitiveCombination3A0XA
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif

        internal PrimitiveCombination3A0XA1 (Primitive3 procedure, Argument0 arg0, SCode arg1, Argument1 arg2)
            : base (procedure, arg0, arg1, arg2)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg1);
            ratorHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveCombination3A0XA1";
#endif
            object ev2 = environment.Argument1Value;

            object ev1;
            Environment env = environment;
            Control unev = this.arg1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3A0XA1";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame1 (this, environment, ev2));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, environment.Argument0Value, ev1, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3A0XQ : PrimitiveCombination3A0
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand2Value;
        internal PrimitiveCombination3A0XQ (Primitive3 procedure, Argument0 arg0, SCode arg1, Quotation arg2)
            : base (procedure, arg0, arg1, arg2)
        {
            this.rand2Value = arg2.Quoted;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg1);
            ratorHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveCombination3A0XQ";
#endif
            object ev2 = this.rand2Value;

            object ev1;
            Environment env = environment;
            Control unev = this.arg1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3A0XQ";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame1 (this, environment, ev2));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, environment.Argument0Value, ev1, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class PrimitiveCombination3A1 : PrimitiveCombination3A
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand2TypeHistogram = new Histogram<Type> ();
#endif

        protected PrimitiveCombination3A1 (Primitive3 procedure, Argument1 arg0, SCode arg1, SCode arg2)
            : base (procedure, arg0, arg1, arg2)
        {
        }

        public static SCode Make (Primitive3 procedure, Argument1 arg0, SCode arg1, SCode arg2)
        {
            return
                (arg1 is Argument) ? PrimitiveCombination3A1A.Make (procedure, arg0, (Argument) arg1, arg2) :
                (arg1 is Quotation) ? PrimitiveCombination3A1Q.Make (procedure, arg0, (Quotation) arg1, arg2) :
                (arg1 is StaticVariable) ? PrimitiveCombination3A1S.Make (procedure, arg0, (StaticVariable) arg1, arg2) :
                (arg2 is StaticVariable) ? new PrimitiveCombination3A1XS (procedure, arg0, arg1, (StaticVariable) arg2) :
                new PrimitiveCombination3A1 (procedure, arg0, arg1, arg2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg1);
            NoteCalls (this.arg2);
            ratorHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            rand2TypeHistogram.Note (this.rand2Type);
            SCode.location = "PrimitiveCombination3A1";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3A1";
#endif
            if (ev2 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object ev1;
            env = environment;
            unev = this.arg1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3A1";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame1 (this, environment, ev2));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, environment.Argument1Value, ev1, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class PrimitiveCombination3A1A : PrimitiveCombination3A1
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand2TypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Offset;

        protected PrimitiveCombination3A1A (Primitive3 procedure, Argument1 arg0, Argument arg1, SCode arg2)
            : base (procedure, arg0, arg1, arg2)
        {
            this.rand1Offset = arg1.Offset;
        }

        public static SCode Make (Primitive3 procedure, Argument1 arg0, Argument arg1, SCode arg2)
        {
            return
                (arg1 is Argument0) ? PrimitiveCombination3A1A0.Make (procedure, arg0, (Argument0) arg1, arg2) :
                (arg1 is Argument1) ? PrimitiveCombination3A1A1.Make (procedure, arg0, (Argument1) arg1, arg2) :
                (arg2 is Argument) ? PrimitiveCombination3A1AA.Make (procedure, arg0, arg1, (Argument) arg2) :
                new PrimitiveCombination3A1A (procedure, arg0, arg1, arg2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg2);
            ratorHistogram.Note (this.procedure);
            rand2TypeHistogram.Note (this.rand2Type);
            SCode.location = "PrimitiveCombination3A1A";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3A1A";
#endif
            if (ev2 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, environment.Argument1Value, environment.ArgumentValue(this.rand1Offset), ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class PrimitiveCombination3A1AA : PrimitiveCombination3A1A
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
#endif
        public readonly int rand2Offset;

        protected PrimitiveCombination3A1AA (Primitive3 procedure, Argument1 arg0, Argument arg1, Argument arg2)
            : base (procedure, arg0, arg1, arg2)
        {
            this.rand2Offset = arg2.Offset;
        }

        public static SCode Make (Primitive3 procedure, Argument1 arg0, Argument arg1, Argument arg2)
        {
            return
                (arg2 is Argument0) ? new PrimitiveCombination3A1AA0 (procedure, arg0,  arg1, (Argument0) arg2) :
                (arg2 is Argument1) ? new PrimitiveCombination3A1AA1 (procedure, arg0,  arg1, (Argument1) arg2) :
                new PrimitiveCombination3A1AA (procedure, arg0, arg1, arg2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.procedure);
            SCode.location = "PrimitiveCombination3A1AA";
#endif

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, environment.Argument1Value, environment.ArgumentValue (this.rand1Offset), environment.ArgumentValue(this.rand2Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3A1AA0 : PrimitiveCombination3A1AA
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
#endif

        internal PrimitiveCombination3A1AA0 (Primitive3 procedure, Argument1 arg0, Argument arg1, Argument0 arg2)
            : base (procedure, arg0, arg1, arg2)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.procedure);
            SCode.location = "PrimitiveCombination3A1AA0";
#endif

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, environment.Argument1Value, environment.ArgumentValue (this.rand1Offset), environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3A1AA1 : PrimitiveCombination3A1AA
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
#endif

        internal PrimitiveCombination3A1AA1 (Primitive3 procedure, Argument1 arg0, Argument arg1, Argument1 arg2)
            : base (procedure, arg0, arg1, arg2)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.procedure);
            SCode.location = "PrimitiveCombination3A1AA1";
#endif

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, environment.Argument1Value, environment.ArgumentValue (this.rand1Offset), environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class PrimitiveCombination3A1A0 : PrimitiveCombination3A1A
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand2TypeHistogram = new Histogram<Type> ();
#endif

        protected PrimitiveCombination3A1A0 (Primitive3 procedure, Argument1 arg0, Argument0 arg1, SCode arg2)
            : base (procedure, arg0, arg1, arg2)
        {
        }

        public static SCode Make (Primitive3 procedure, Argument1 arg0, Argument0 arg1, SCode arg2)
        {
            return
                new PrimitiveCombination3A1A0 (procedure, arg0, arg1, arg2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg2);
            ratorHistogram.Note (this.procedure);
            rand2TypeHistogram.Note (this.rand2Type);
            SCode.location = "PrimitiveCombination3A1A0";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3A1A0";
#endif
            if (ev2 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, environment.Argument1Value, environment.Argument0Value, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class PrimitiveCombination3A1A1 : PrimitiveCombination3A1A
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand2TypeHistogram = new Histogram<Type> ();
#endif

        protected PrimitiveCombination3A1A1 (Primitive3 procedure, Argument1 arg0, Argument1 arg1, SCode arg2)
            : base (procedure, arg0, arg1, arg2)
        {
        }

        public static SCode Make (Primitive3 procedure, Argument1 arg0, Argument1 arg1, SCode arg2)
        {
            return
                new PrimitiveCombination3A1A1 (procedure, arg0, arg1, arg2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg2);
            ratorHistogram.Note (this.procedure);
            rand2TypeHistogram.Note (this.rand2Type);
            SCode.location = "PrimitiveCombination3A1A1";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3A1A1";
#endif
            if (ev2 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, environment.Argument1Value, environment.Argument1Value, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class PrimitiveCombination3A1Q : PrimitiveCombination3A1
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand2TypeHistogram = new Histogram<Type> ();
#endif

        public readonly object rand1Value;
        protected PrimitiveCombination3A1Q (Primitive3 procedure, Argument1 arg0, Quotation arg1, SCode arg2)
            : base (procedure, arg0, arg1, arg2)
        {
            this.rand1Value = arg1.Quoted;
        }

        public static SCode Make (Primitive3 procedure, Argument1 arg0, Quotation arg1, SCode arg2)
        {
            return
                new PrimitiveCombination3A1Q (procedure, arg0, arg1, arg2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg2);
            ratorHistogram.Note (this.procedure);
            rand2TypeHistogram.Note (this.rand2Type);
            SCode.location = "PrimitiveCombination3A1Q";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3A1Q";
#endif
            if (ev2 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, environment.Argument1Value, this.rand1Value, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class PrimitiveCombination3A1S : PrimitiveCombination3A1
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand2TypeHistogram = new Histogram<Type> ();
#endif

        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        protected PrimitiveCombination3A1S (Primitive3 procedure, Argument1 arg0, StaticVariable arg1, SCode arg2)
            : base (procedure, arg0, arg1, arg2)
        {
            this.rand1Name = arg1.Name;
            this.rand1Offset = arg1.Offset;
        }

        public static SCode Make (Primitive3 procedure, Argument1 arg0, StaticVariable arg1, SCode arg2)
        {
            return
                new PrimitiveCombination3A1S (procedure, arg0, arg1, arg2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg2);
            ratorHistogram.Note (this.procedure);
            rand2TypeHistogram.Note (this.rand2Type);
            SCode.location = "PrimitiveCombination3A1S";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3A1Q";
#endif
            if (ev2 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, environment.Argument1Value, ev1, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3A1XS : PrimitiveCombination3A1
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif

        public readonly Symbol rand2Name;
        public readonly int rand2Offset;

        internal PrimitiveCombination3A1XS (Primitive3 procedure, Argument1 arg0, SCode arg1, StaticVariable arg2)
            : base (procedure, arg0, arg1, arg2)
        {
            this.rand2Name = arg2.Name;
            this.rand2Offset = arg2.Offset;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg1);
            ratorHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveCombination3A1XS";
#endif

            object ev2;
            if (environment.StaticValue (out ev2, this.rand2Name, this.rand2Offset))
                throw new NotImplementedException ();

            object ev1;
            Environment env = environment;
            Control unev = this.arg1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3A1XS";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, environment.Argument1Value, ev1, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class PrimitiveCombination3AQ : PrimitiveCombination3A
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand2TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Value;

        protected PrimitiveCombination3AQ (Primitive3 procedure, Argument arg0, Quotation arg1, SCode arg2)
            : base (procedure, arg0, arg1, arg2)
        {
            this.rand1Value = arg1.Quoted;
        }

        public static SCode Make (Primitive3 procedure, Argument arg0, Quotation arg1, SCode arg2)
        {
            return
                (arg2 is Argument) ? PrimitiveCombination3AQA.Make (procedure, arg0, arg1, (Argument) arg2) :
                new PrimitiveCombination3AQ (procedure, arg0, arg1, arg2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg2);
            ratorHistogram.Note (this.procedure);
            rand2TypeHistogram.Note (this.rand2Type);
            SCode.location = "PrimitiveCombination3AQ";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3AQ";
#endif
            if (ev2 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class PrimitiveCombination3AQA : PrimitiveCombination3AQ
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
#endif
        public readonly int rand2Offset;

        protected PrimitiveCombination3AQA (Primitive3 procedure, Argument arg0, Quotation arg1, Argument arg2)
            : base (procedure, arg0, arg1, arg2)
        {
        this.rand2Offset = arg2.Offset;
        }

        public static SCode Make (Primitive3 procedure, Argument arg0, Quotation arg1, Argument arg2)
        {
            return
                (arg2 is Argument0) ? new PrimitiveCombination3AQA0 (procedure, arg0, arg1, (Argument0) arg2) :
                (arg2 is Argument1) ? new PrimitiveCombination3AQA1 (procedure, arg0, arg1, (Argument1) arg2) :
                new PrimitiveCombination3AQA (procedure, arg0, arg1, arg2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.procedure);
            SCode.location = "PrimitiveCombination3AQA";
#endif

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value, environment.ArgumentValue(this.rand2Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3AQA0 : PrimitiveCombination3AQA
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
#endif

        internal PrimitiveCombination3AQA0 (Primitive3 procedure, Argument arg0, Quotation arg1, Argument0 arg2)
            : base (procedure, arg0, arg1, arg2)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.procedure);
            SCode.location = "PrimitiveCombination3AQA0";
#endif

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3AQA1 : PrimitiveCombination3AQA
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
#endif

        internal PrimitiveCombination3AQA1 (Primitive3 procedure, Argument arg0, Quotation arg1, Argument1 arg2)
            : base (procedure, arg0, arg1, arg2)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.procedure);
            SCode.location = "PrimitiveCombination3AQA1";
#endif

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, environment.ArgumentValue (this.rand0Offset), this.rand1Value, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class PrimitiveCombination3Q : PrimitiveCombination3
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand2TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand0Value;

        protected PrimitiveCombination3Q (Primitive3 procedure, Quotation arg0, SCode arg1, SCode arg2)
            : base (procedure, arg0, arg1, arg2)
        {
            this.rand0Value = arg0.Quoted;
        }

        public static SCode Make (Primitive3 procedure, Quotation arg0, SCode arg1, SCode arg2)
        {
            return
                (arg1 is Argument) ? PrimitiveCombination3QA.Make (procedure, arg0, (Argument) arg1, arg2) :
                (arg1 is Quotation) ? PrimitiveCombination3QQ.Make (procedure, arg0, (Quotation) arg1, arg2) :
                (arg1 is StaticVariable) ? PrimitiveCombination3QS.Make (procedure, arg0, (StaticVariable) arg1, arg2) :
                (arg2 is Argument) ? PrimitiveCombination3QXA.Make (procedure, arg0, arg1, (Argument) arg2) :
                (arg2 is StaticVariable) ? PrimitiveCombination3QXS.Make (procedure, arg0, arg1, (StaticVariable) arg2) :
                new PrimitiveCombination3Q (procedure, arg0, arg1, arg2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg1);
            NoteCalls (this.arg2);
            ratorHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            rand2TypeHistogram.Note (this.rand2Type);
            SCode.location = "PrimitiveCombination3Q";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3Q";
#endif
            if (ev2 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object ev1;
            env = environment;
            unev = this.arg1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3Q";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame1 (this, environment, ev2));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, this.rand0Value, ev1, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
   }

    [Serializable]
    class PrimitiveCombination3QA : PrimitiveCombination3Q
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand2TypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Offset;

        protected PrimitiveCombination3QA (Primitive3 procedure, Quotation arg0, Argument arg1, SCode arg2)
            : base (procedure, arg0, arg1, arg2)
        {
            this.rand1Offset = arg1.Offset;
        }

        public static SCode Make (Primitive3 procedure, Quotation arg0, Argument arg1, SCode arg2)
        {
            return
                (arg1 is Argument0) ? PrimitiveCombination3QA0.Make (procedure, arg0, (Argument0) arg1, arg2) :
                (arg1 is Argument1) ? PrimitiveCombination3QA1.Make (procedure, arg0, (Argument1) arg1, arg2) :
                new PrimitiveCombination3QA (procedure, arg0, arg1, arg2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg2);
            ratorHistogram.Note (this.procedure);
            rand2TypeHistogram.Note (this.rand2Type);
            SCode.location = "PrimitiveCombination3QA";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3QA";
#endif
            if (ev2 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, this.rand0Value, environment.ArgumentValue(this.rand1Offset), ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class PrimitiveCombination3QA0 : PrimitiveCombination3QA
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand2TypeHistogram = new Histogram<Type> ();
#endif
        protected PrimitiveCombination3QA0 (Primitive3 procedure, Quotation arg0, Argument0 arg1, SCode arg2)
            : base (procedure, arg0, arg1, arg2)
        {
        }

        public static SCode Make (Primitive3 procedure, Quotation arg0, Argument0 arg1, SCode arg2)
        {
            return
                (arg2 is Argument) ? PrimitiveCombination3QA0A.Make (procedure, arg0, arg1, (Argument) arg2) :
                new PrimitiveCombination3QA0 (procedure, arg0, arg1, arg2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg2);
            ratorHistogram.Note (this.procedure);
            rand2TypeHistogram.Note (this.rand2Type);
            SCode.location = "PrimitiveCombination3QA0";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3QA0";
#endif
            if (ev2 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, this.rand0Value, environment.Argument0Value, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class PrimitiveCombination3QA0A : PrimitiveCombination3QA0
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
#endif
        public readonly int rand2Offset;
        protected PrimitiveCombination3QA0A (Primitive3 procedure, Quotation arg0, Argument0 arg1, Argument arg2)
            : base (procedure, arg0, arg1, arg2)
        {
            this.rand2Offset = arg2.Offset;
        }

        public static SCode Make (Primitive3 procedure, Quotation arg0, Argument0 arg1, Argument arg2)
        {
            return
                (arg2 is Argument0) ? new PrimitiveCombination3QA0A0 (procedure, arg0, arg1, (Argument0) arg2) :
                (arg2 is Argument1) ? new PrimitiveCombination3QA0A1 (procedure, arg0, arg1, (Argument1) arg2) :
                new PrimitiveCombination3QA0A (procedure, arg0, arg1, arg2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.procedure);
            SCode.location = "PrimitiveCombination3QA0A";
#endif

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, this.rand0Value, environment.Argument0Value, environment.ArgumentValue(this.rand2Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3QA0A0 : PrimitiveCombination3QA0A
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
#endif
        internal PrimitiveCombination3QA0A0 (Primitive3 procedure, Quotation arg0, Argument0 arg1, Argument0 arg2)
            : base (procedure, arg0, arg1, arg2)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.procedure);
            SCode.location = "PrimitiveCombination3QA0A0";
#endif

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, this.rand0Value, environment.Argument0Value, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3QA0A1 : PrimitiveCombination3QA0A
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
#endif
        internal PrimitiveCombination3QA0A1 (Primitive3 procedure, Quotation arg0, Argument0 arg1, Argument1 arg2)
            : base (procedure, arg0, arg1, arg2)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.procedure);
            SCode.location = "PrimitiveCombination3QA0A1";
#endif

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, this.rand0Value, environment.Argument0Value, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class PrimitiveCombination3QA1 : PrimitiveCombination3QA
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand2TypeHistogram = new Histogram<Type> ();
#endif
        protected PrimitiveCombination3QA1 (Primitive3 procedure, Quotation arg0, Argument1 arg1, SCode arg2)
            : base (procedure, arg0, arg1, arg2)
        {
        }

        public static SCode Make (Primitive3 procedure, Quotation arg0, Argument1 arg1, SCode arg2)
        {
            return
                new PrimitiveCombination3QA1 (procedure, arg0, arg1, arg2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg2);
            ratorHistogram.Note (this.procedure);
            rand2TypeHistogram.Note (this.rand2Type);
            SCode.location = "PrimitiveCombination3QA1";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3QA1";
#endif
            if (ev2 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, this.rand0Value, environment.Argument1Value, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class PrimitiveCombination3QQ : PrimitiveCombination3Q
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand2TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Value;

        protected PrimitiveCombination3QQ (Primitive3 procedure, Quotation arg0, Quotation arg1, SCode arg2)
            : base (procedure, arg0, arg1, arg2)
        {
            this.rand1Value = arg1.Quoted;
        }

        public static SCode Make (Primitive3 procedure, Quotation arg0, Quotation arg1, SCode arg2)
        {
            return
                (arg2 is Argument) ? PrimitiveCombination3QQA.Make (procedure, arg0, arg1, (Argument) arg2) :
                (arg2 is Quotation) ? new PrimitiveCombination3QQQ (procedure, arg0, arg1, (Quotation) arg2) :
                new PrimitiveCombination3QQ (procedure, arg0, arg1, arg2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg2);
            ratorHistogram.Note (this.procedure);
            rand2TypeHistogram.Note (this.rand2Type);
            SCode.location = "PrimitiveCombination3QQ";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3QQ";
#endif
            if (ev2 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, this.rand0Value, this.rand1Value, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class PrimitiveCombination3QQA : PrimitiveCombination3QQ
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
#endif
        public readonly int rand2Offset;

        protected PrimitiveCombination3QQA (Primitive3 procedure, Quotation arg0, Quotation arg1, Argument arg2)
            : base (procedure, arg0, arg1, arg2)
        {
            this.rand2Offset = arg2.Offset;
        }

        public static SCode Make (Primitive3 procedure, Quotation arg0, Quotation arg1, Argument arg2)
        {
            return
                (arg2 is Argument0) ? new PrimitiveCombination3QQA0 (procedure, arg0, arg1, (Argument0) arg2) :
                (arg2 is Argument1) ? new PrimitiveCombination3QQA1 (procedure, arg0, arg1, (Argument1) arg2) :
                new PrimitiveCombination3QQA (procedure, arg0, arg1, arg2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.procedure);
            SCode.location = "PrimitiveCombination3QQA";
#endif

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, this.rand0Value, this.rand1Value, environment.ArgumentValue(this.rand2Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3QQA0 : PrimitiveCombination3QQA
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
#endif

        internal PrimitiveCombination3QQA0 (Primitive3 procedure, Quotation arg0, Quotation arg1, Argument0 arg2)
            : base (procedure, arg0, arg1, arg2)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.procedure);
            SCode.location = "PrimitiveCombination3QQA0";
#endif

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, this.rand0Value, this.rand1Value, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3QQA1 : PrimitiveCombination3QQA
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
#endif

        internal PrimitiveCombination3QQA1 (Primitive3 procedure, Quotation arg0, Quotation arg1, Argument1 arg2)
            : base (procedure, arg0, arg1, arg2)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.procedure);
            SCode.location = "PrimitiveCombination3QQA1";
#endif

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, this.rand0Value, this.rand1Value, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3QQQ : PrimitiveCombination3QQ
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
#endif
        public readonly object rand2Value;

        internal PrimitiveCombination3QQQ (Primitive3 procedure, Quotation arg0, Quotation arg1, Quotation arg2)
            : base (procedure, arg0, arg1, arg2)
        {
            this.rand2Value = arg2.Quoted;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("PrimitiveCombination3QQQ");

            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, this.rand0Value, this.rand1Value, this.rand2Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class PrimitiveCombination3QS : PrimitiveCombination3Q
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand2TypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        protected PrimitiveCombination3QS (Primitive3 procedure, Quotation arg0, StaticVariable arg1, SCode arg2)
            : base (procedure, arg0, arg1, arg2)
        {
            this.rand1Name = arg1.Name;
            this.rand1Offset = arg1.Offset;
        }

        public static SCode Make (Primitive3 procedure, Quotation arg0, StaticVariable arg1, SCode arg2)
        {
            return
                (arg2 is StaticVariable) ? new PrimitiveCombination3QSS (procedure, arg0, arg1, (StaticVariable) arg2) :
                new PrimitiveCombination3QS (procedure, arg0, arg1, arg2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg2);
            ratorHistogram.Note (this.procedure);
            rand2TypeHistogram.Note (this.rand2Type);
            SCode.location = "PrimitiveCombination3QS";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3QS";
#endif
            if (ev2 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, this.rand0Value, ev1, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3QSS : PrimitiveCombination3QS
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
#endif
        public readonly Symbol rand2Name;
        public readonly int rand2Offset;

        internal PrimitiveCombination3QSS (Primitive3 procedure, Quotation arg0, StaticVariable arg1, StaticVariable arg2)
            : base (procedure, arg0, arg1, arg2)
        {
            this.rand2Name = arg2.Name;
            this.rand2Offset = arg2.Offset;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.procedure);
            SCode.location = "PrimitiveCombination3QSS";
#endif
            object ev2;
            if (environment.StaticValue (out ev2, this.rand2Name, this.rand2Offset))
                throw new NotImplementedException ();

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, this.rand0Value, ev1, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class PrimitiveCombination3QXA : PrimitiveCombination3Q
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand2Offset;

        protected PrimitiveCombination3QXA (Primitive3 procedure, Quotation arg0, SCode arg1, Argument arg2)
            : base (procedure, arg0, arg1, arg2)
        {
            this.rand2Offset = arg2.Offset;
        }

        public static SCode Make (Primitive3 procedure, Quotation arg0, SCode arg1, Argument arg2)
        {
            return
                (arg2 is Argument0) ? new PrimitiveCombination3QXA0 (procedure, arg0, arg1, (Argument0) arg2) :
                (arg2 is Argument1) ? new PrimitiveCombination3QXA1 (procedure, arg0, arg1, (Argument1) arg2) :
                new PrimitiveCombination3QXA (procedure, arg0, arg1, arg2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg1);
            ratorHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveCombination3QXA";
#endif
            object ev2 = environment.ArgumentValue(this.rand2Offset);

            object ev1;
            Environment env = environment;
            Control unev = this.arg1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3QXA";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame1 (this, environment, ev2));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, this.rand0Value, ev1, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3QXA0 : PrimitiveCombination3QXA
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        internal PrimitiveCombination3QXA0 (Primitive3 procedure, Quotation arg0, SCode arg1, Argument0 arg2)
            : base (procedure, arg0, arg1, arg2)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg1);
            ratorHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveCombination3QXA0";
#endif
            object ev2 = environment.Argument0Value;

            object ev1;
            Environment env = environment;
            Control unev = this.arg1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3QXA0";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame1 (this, environment, ev2));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, this.rand0Value, ev1, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3QXA1 : PrimitiveCombination3QXA
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        internal PrimitiveCombination3QXA1 (Primitive3 procedure, Quotation arg0, SCode arg1, Argument1 arg2)
            : base (procedure, arg0, arg1, arg2)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg1);
            ratorHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveCombination3QXA1";
#endif
            object ev2 = environment.Argument1Value;

            object ev1;
            Environment env = environment;
            Control unev = this.arg1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3QXA1";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame1 (this, environment, ev2));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, this.rand0Value, ev1, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class PrimitiveCombination3QXS : PrimitiveCombination3Q
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand2Name;
        public readonly int rand2Offset;

        protected PrimitiveCombination3QXS (Primitive3 procedure, Quotation arg0, SCode arg1, StaticVariable arg2)
            : base (procedure, arg0, arg1, arg2)
        {
            this.rand2Name = arg2.Name;
            this.rand2Offset = arg2.Offset;
        }

        public static SCode Make (Primitive3 procedure, Quotation arg0, SCode arg1, StaticVariable arg2)
        {
            return
                new PrimitiveCombination3QXS (procedure, arg0, arg1, arg2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg1);
            ratorHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveCombination3QXS";
#endif
            object ev2;
            if (environment.StaticValue (out ev2, this.rand2Name, this.rand2Offset))
                throw new NotImplementedException ();

            object ev1;
            Environment env = environment;
            Control unev = this.arg1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3QXS";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame1 (this, environment, ev2));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, this.rand0Value, ev1, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class PrimitiveCombination3S : PrimitiveCombination3
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand2TypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand0Name;
        public readonly int rand0Offset;

        protected PrimitiveCombination3S (Primitive3 procedure, StaticVariable arg0, SCode arg1, SCode arg2)
            : base (procedure, arg0, arg1, arg2)
        {
            this.rand0Name = arg0.Name;
            this.rand0Offset = arg0.Offset;
        }

        public static SCode Make (Primitive3 procedure, StaticVariable arg0, SCode arg1, SCode arg2)
        {
            return
                (arg1 is Argument) ? PrimitiveCombination3SA.Make (procedure, arg0, (Argument) arg1, arg2) :
                (arg1 is Quotation) ? PrimitiveCombination3SQ.Make (procedure, arg0, (Quotation) arg1, arg2) :
                (arg1 is StaticVariable) ? PrimitiveCombination3SS.Make (procedure, arg0, (StaticVariable) arg1, arg2) :
                (arg2 is Argument) ? PrimitiveCombination3SXA.Make (procedure, arg0, arg1, (Argument) arg2) :
                (arg2 is Quotation) ? new PrimitiveCombination3SXQ (procedure, arg0, arg1, (Quotation) arg2) :
                (arg2 is StaticVariable) ? new PrimitiveCombination3SXS (procedure, arg0, arg1, (StaticVariable) arg2) :
                new PrimitiveCombination3S (procedure, arg0, arg1, arg2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg1);
            NoteCalls (this.arg2);
            ratorHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            rand2TypeHistogram.Note (this.rand2Type);
            SCode.location = "PrimitiveCombination3S";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3S";
#endif
            if (ev2 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object ev1;
            env = environment;
            unev = this.arg1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3S";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame1 (this, environment, ev2));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, ev1, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class PrimitiveCombination3SA : PrimitiveCombination3S
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand2TypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Offset;

        protected PrimitiveCombination3SA (Primitive3 procedure, StaticVariable arg0, Argument arg1, SCode arg2)
            : base (procedure, arg0, arg1, arg2)
        {
            this.rand1Offset = arg1.Offset;
        }

        public static SCode Make (Primitive3 procedure, StaticVariable arg0, Argument arg1, SCode arg2)
        {
            return
                (arg1 is Argument0) ? PrimitiveCombination3SA0.Make (procedure, arg0, (Argument0) arg1, arg2) :
                (arg1 is Argument1) ? PrimitiveCombination3SA1.Make (procedure, arg0, (Argument1) arg1, arg2) :
                new PrimitiveCombination3SA (procedure, arg0, arg1, arg2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg2);
            ratorHistogram.Note (this.procedure);
            rand2TypeHistogram.Note (this.rand2Type);
            SCode.location = "PrimitiveCombination3SA";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3SA";
#endif
            if (ev2 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue(this.rand1Offset), ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class PrimitiveCombination3SA0 : PrimitiveCombination3SA
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand2TypeHistogram = new Histogram<Type> ();
#endif

        protected PrimitiveCombination3SA0 (Primitive3 procedure, StaticVariable arg0, Argument0 arg1, SCode arg2)
            : base (procedure, arg0, arg1, arg2)
        {
        }

        public static SCode Make (Primitive3 procedure, StaticVariable arg0, Argument0 arg1, SCode arg2)
        {
            return
                (arg2 is StaticVariable)? new PrimitiveCombination3SA0S (procedure, arg0, arg1, (StaticVariable) arg2) :
                new PrimitiveCombination3SA0 (procedure, arg0, arg1, arg2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg2);
            ratorHistogram.Note (this.procedure);
            rand2TypeHistogram.Note (this.rand2Type);
            SCode.location = "PrimitiveCombination3SA0";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3SA0";
#endif
            if (ev2 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, environment.Argument0Value, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class PrimitiveCombination3SA0S : PrimitiveCombination3SA0
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
#endif
        public readonly Symbol rand2Name;
        public readonly int rand2Offset;
        internal PrimitiveCombination3SA0S (Primitive3 procedure, StaticVariable arg0, Argument0 arg1, StaticVariable arg2)
            : base (procedure, arg0, arg1, arg2)
        {
            this.rand2Name = arg2.Name;
            this.rand2Offset = arg2.Offset;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.procedure);
            SCode.location = "PrimitiveCombination3SA0S";
#endif
            object ev2;
            if (environment.StaticValue (out ev2, this.rand2Name, this.rand2Offset))
                throw new NotImplementedException ();

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, environment.Argument0Value, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class PrimitiveCombination3SA1 : PrimitiveCombination3SA
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand2TypeHistogram = new Histogram<Type> ();
#endif

        protected PrimitiveCombination3SA1 (Primitive3 procedure, StaticVariable arg0, Argument1 arg1, SCode arg2)
            : base (procedure, arg0, arg1, arg2)
        {
        }

        public static SCode Make (Primitive3 procedure, StaticVariable arg0, Argument1 arg1, SCode arg2)
        {
            return
                (arg2 is Argument) ? PrimitiveCombination3SA1A.Make (procedure, arg0, arg1, (Argument) arg2) :
                new PrimitiveCombination3SA1 (procedure, arg0, arg1, arg2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg2);
            ratorHistogram.Note (this.procedure);
            rand2TypeHistogram.Note (this.rand2Type);
            SCode.location = "PrimitiveCombination3SA1";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3SA1";
#endif
            if (ev2 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, environment.Argument1Value, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class PrimitiveCombination3SA1A : PrimitiveCombination3SA1
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
#endif
        public readonly int rand2Offset;
        protected PrimitiveCombination3SA1A (Primitive3 procedure, StaticVariable arg0, Argument1 arg1, Argument arg2)
            : base (procedure, arg0, arg1, arg2)
        {
        this.rand2Offset = arg2.Offset;
        }

        public static SCode Make (Primitive3 procedure, StaticVariable arg0, Argument1 arg1, Argument arg2)
        {
            return
                (arg2 is Argument0) ? new PrimitiveCombination3SA1A0 (procedure, arg0, arg1, (Argument0) arg2) :
                (arg2 is Argument1) ? new PrimitiveCombination3SA1A1 (procedure, arg0, arg1, (Argument1) arg2) :
                new PrimitiveCombination3SA1A (procedure, arg0, arg1, arg2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg2);
            ratorHistogram.Note (this.procedure);
            SCode.location = "PrimitiveCombination3SA1A";
#endif

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, environment.Argument1Value, environment.ArgumentValue(this.rand2Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3SA1A0 : PrimitiveCombination3SA1A
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
#endif

        internal PrimitiveCombination3SA1A0 (Primitive3 procedure, StaticVariable arg0, Argument1 arg1, Argument0 arg2)
            : base (procedure, arg0, arg1, arg2)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.procedure);
            SCode.location = "PrimitiveCombination3SA1A0";
#endif

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, environment.Argument1Value, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3SA1A1 : PrimitiveCombination3SA1A
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
#endif

        internal PrimitiveCombination3SA1A1 (Primitive3 procedure, StaticVariable arg0, Argument1 arg1, Argument1 arg2)
            : base (procedure, arg0, arg1, arg2)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg2);
            ratorHistogram.Note (this.procedure);
            SCode.location = "PrimitiveCombination3SA1A1";
#endif

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, environment.Argument1Value, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class PrimitiveCombination3SQ : PrimitiveCombination3S
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand2TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Value;

        protected PrimitiveCombination3SQ (Primitive3 procedure, StaticVariable arg0, Quotation arg1, SCode arg2)
            : base (procedure, arg0, arg1, arg2)
        {
            this.rand1Value = arg1.Quoted;
        }

        public static SCode Make (Primitive3 procedure, StaticVariable arg0, Quotation arg1, SCode arg2)
        {
            return
                (arg2 is Argument) ? PrimitiveCombination3SQA.Make (procedure, arg0, arg1, (Argument) arg2) :
                (arg2 is Quotation) ? new PrimitiveCombination3SQQ (procedure, arg0, arg1, (Quotation) arg2) :
                (arg2 is StaticVariable) ? new PrimitiveCombination3SQS (procedure, arg0, arg1, (StaticVariable) arg2) :
                new PrimitiveCombination3SQ (procedure, arg0, arg1, arg2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg2);
            ratorHistogram.Note (this.procedure);
            rand2TypeHistogram.Note (this.rand2Type);
            SCode.location = "PrimitiveCombination3SQ";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3SQ";
#endif
            if (ev2 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, this.rand1Value, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class PrimitiveCombination3SQA : PrimitiveCombination3SQ
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
#endif
        public readonly int rand2Offset;

        protected PrimitiveCombination3SQA (Primitive3 procedure, StaticVariable arg0, Quotation arg1, Argument arg2)
            : base (procedure, arg0, arg1, arg2)
        {
            this.rand2Offset = arg2.Offset;
        }

        public static SCode Make (Primitive3 procedure, StaticVariable arg0, Quotation arg1, Argument arg2)
        {
            return
                (arg2 is Argument0) ? new PrimitiveCombination3SQA0 (procedure, arg0, arg1, (Argument0) arg2) :
                (arg2 is Argument1) ? new PrimitiveCombination3SQA1 (procedure, arg0, arg1, (Argument1) arg2) :
                new PrimitiveCombination3SQA (procedure, arg0, arg1, arg2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.procedure);
            SCode.location = "PrimitiveCombination3SQA";
#endif
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, this.rand1Value, environment.ArgumentValue(this.rand2Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3SQA0 : PrimitiveCombination3SQA
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
#endif

        internal PrimitiveCombination3SQA0 (Primitive3 procedure, StaticVariable arg0, Quotation arg1, Argument0 arg2)
            : base (procedure, arg0, arg1, arg2)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.procedure);
            SCode.location = "PrimitiveCombination3SQA0";
#endif
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, this.rand1Value, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3SQA1 : PrimitiveCombination3SQA
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
#endif
        internal PrimitiveCombination3SQA1 (Primitive3 procedure, StaticVariable arg0, Quotation arg1, Argument1 arg2)
            : base (procedure, arg0, arg1, arg2)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.procedure);
            SCode.location = "PrimitiveCombination3SQA1";
#endif
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, this.rand1Value, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3SQQ : PrimitiveCombination3SQ
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
#endif
        public readonly object rand2Value;

        internal PrimitiveCombination3SQQ (Primitive3 procedure, StaticVariable arg0, Quotation arg1, Quotation arg2)
            : base (procedure, arg0, arg1, arg2)
        {
            this.rand2Value = arg2.Quoted;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.procedure);
            SCode.location = "PrimitiveCombination3SQQ";
#endif
            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, this.rand1Value, this.rand2Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3SQS : PrimitiveCombination3SQ
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
#endif
        public readonly Symbol rand2Name;
        public readonly int rand2Offset;

        internal PrimitiveCombination3SQS (Primitive3 procedure, StaticVariable arg0, Quotation arg1, StaticVariable arg2)
            : base (procedure, arg0, arg1, arg2)
        {
            this.rand2Name = arg2.Name;
            this.rand2Offset = arg2.Offset;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.procedure);
            SCode.location = "PrimitiveCombination3SQS";
#endif
            object ev2;
            if (environment.StaticValue (out ev2, this.rand2Name, this.rand2Offset))
                throw new NotImplementedException ();

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, this.rand1Value, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class PrimitiveCombination3SS : PrimitiveCombination3S
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand2TypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        protected PrimitiveCombination3SS (Primitive3 procedure, StaticVariable arg0, StaticVariable arg1, SCode arg2)
            : base (procedure, arg0, arg1, arg2)
        {
            this.rand1Name = arg1.Name;
            this.rand1Offset = arg1.Offset;
        }

        public static SCode Make (Primitive3 procedure, StaticVariable arg0, StaticVariable arg1, SCode arg2)
        {
            return
                (arg2 is Argument) ? PrimitiveCombination3SSA.Make (procedure, arg0, arg1, (Argument) arg2):
                (arg2 is StaticVariable) ? new PrimitiveCombination3SSS (procedure, arg0, arg1, (StaticVariable) arg2) :
                new PrimitiveCombination3SS (procedure, arg0, arg1, arg2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg2);
            ratorHistogram.Note (this.procedure);
            rand2TypeHistogram.Note (this.rand2Type);
            SCode.location = "PrimitiveCombination3SS";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3SS";
#endif
            if (ev2 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, ev1, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class PrimitiveCombination3SSA : PrimitiveCombination3SS
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
#endif
        public readonly int rand2Offset;

        protected PrimitiveCombination3SSA (Primitive3 procedure, StaticVariable arg0, StaticVariable arg1, Argument arg2)
            : base (procedure, arg0, arg1, arg2)
        {
            this.rand2Offset = arg2.Offset;
        }

        public static SCode Make (Primitive3 procedure, StaticVariable arg0, StaticVariable arg1, Argument arg2)
        {
            return
                (arg2 is Argument0) ? new PrimitiveCombination3SSA0 (procedure, arg0, arg1, (Argument0) arg2) :
                (arg2 is Argument1) ? new PrimitiveCombination3SSA1 (procedure, arg0, arg1, (Argument1) arg2) :
                new PrimitiveCombination3SSA (procedure, arg0, arg1, arg2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.procedure);
            SCode.location = "PrimitiveCombination3SSA";
#endif
            object ev2 = environment.ArgumentValue (this.rand2Offset);

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, ev1, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3SSA0 : PrimitiveCombination3SSA
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
#endif
        internal PrimitiveCombination3SSA0 (Primitive3 procedure, StaticVariable arg0, StaticVariable arg1, Argument0 arg2)
            : base (procedure, arg0, arg1, arg2)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.procedure);
            SCode.location = "PrimitiveCombination3SSA0";
#endif
            object ev2 = environment.Argument0Value;

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, ev1, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3SSA1 : PrimitiveCombination3SSA
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
#endif
        internal PrimitiveCombination3SSA1 (Primitive3 procedure, StaticVariable arg0, StaticVariable arg1, Argument1 arg2)
            : base (procedure, arg0, arg1, arg2)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.procedure);
            SCode.location = "PrimitiveCombination3SSA1";
#endif
            object ev2 = environment.Argument1Value;

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, ev1, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3SSS : PrimitiveCombination3SS
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
#endif
        public readonly Symbol rand2Name;
        public readonly int rand2Offset;

        internal PrimitiveCombination3SSS (Primitive3 procedure, StaticVariable arg0, StaticVariable arg1, StaticVariable arg2)
            : base (procedure, arg0, arg1, arg2)
        {
            this.rand2Name = arg2.Name;
            this.rand2Offset = arg2.Offset;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            ratorHistogram.Note (this.procedure);
            SCode.location = "PrimitiveCombination3SSS";
#endif
            object ev2;
            if (environment.StaticValue (out ev2, this.rand2Name, this.rand2Offset))
                throw new NotImplementedException ();

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, ev1, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class PrimitiveCombination3SXA : PrimitiveCombination3S
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand2Offset;

        protected PrimitiveCombination3SXA (Primitive3 procedure, StaticVariable arg0, SCode arg1, Argument arg2)
            : base (procedure, arg0, arg1, arg2)
        {
            this.rand2Offset = arg2.Offset;
        }

        static public SCode Make (Primitive3 procedure, StaticVariable arg0, SCode arg1, Argument arg2)
        {
           return
                (arg2 is Argument0) ? new PrimitiveCombination3SXA0 (procedure, arg0, arg1, (Argument0) arg2) :
                (arg2 is Argument1) ? new PrimitiveCombination3SXA1 (procedure, arg0, arg1, (Argument1) arg2) :
                new PrimitiveCombination3SXA (procedure, arg0, arg1, arg2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg1);
            ratorHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveCombination3SXA";
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.arg1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3SXA";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame1 (this, environment, environment.ArgumentValue (this.rand2Offset)));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, ev1, environment.ArgumentValue (this.rand2Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3SXA0 : PrimitiveCombination3SXA
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif

        internal PrimitiveCombination3SXA0 (Primitive3 procedure, StaticVariable arg0, SCode arg1, Argument0 arg2)
            : base (procedure, arg0, arg1, arg2)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg1);
            ratorHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveCombination3SXA0";
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.arg1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3SXA0";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame1 (this, environment, environment.Argument0Value));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, ev1, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3SXA1 : PrimitiveCombination3SXA
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif

        internal PrimitiveCombination3SXA1 (Primitive3 procedure, StaticVariable arg0, SCode arg1, Argument1 arg2)
            : base (procedure, arg0, arg1, arg2)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg1);
            ratorHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveCombination3SXA1";
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.arg1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3SXA1";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame1 (this, environment, environment.Argument1Value));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, ev1, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3SXQ : PrimitiveCombination3S
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand2Value;

        internal PrimitiveCombination3SXQ (Primitive3 procedure, StaticVariable arg0, SCode arg1, Quotation arg2)
            : base (procedure, arg0, arg1, arg2)
        {
            this.rand2Value = arg2.Quoted;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg1);
            ratorHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveCombination3SXQ";
#endif
            object ev1;
            Environment env = environment;
            Control unev = this.arg1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3SXQ";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame1 (this, environment, this.rand2Value));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, ev1, this.rand2Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3SXS : PrimitiveCombination3S
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand2Name;
        public readonly int rand2Offset;

        internal PrimitiveCombination3SXS (Primitive3 procedure, StaticVariable arg0, SCode arg1, StaticVariable arg2)
            : base (procedure, arg0, arg1, arg2)
        {
            this.rand2Name = arg2.Name;
            this.rand2Offset = arg2.Offset;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg1);
            ratorHistogram.Note (this.procedure);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveCombination3SXS";
#endif
            object ev2;
            if (environment.StaticValue (out ev2, this.rand2Name, this.rand2Offset))
                throw new NotImplementedException ();

            object ev1;
            Environment env = environment;
            Control unev = this.arg1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3SXQ";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame1 (this, environment, ev2));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object ev0;
            if (environment.StaticValue (out ev0, this.rand0Name, this.rand0Offset))
                throw new NotImplementedException ();
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, ev1, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class PrimitiveCombination3XA : PrimitiveCombination3
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand2TypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand1Offset;

        protected PrimitiveCombination3XA (Primitive3 procedure, SCode arg0, Argument arg1, SCode arg2)
            : base (procedure, arg0, arg1, arg2)
        {
            this.rand1Offset = arg1.Offset;
        }

        public static SCode Make (Primitive3 procedure, SCode arg0, Argument arg1, SCode arg2)
        {
            return
                (arg1 is Argument0) ? PrimitiveCombination3XA0.Make (procedure, arg0, (Argument0) arg1, arg2) :
                                (arg1 is Argument1) ? PrimitiveCombination3XA1.Make (procedure, arg0, (Argument1) arg1, arg2) :
                new PrimitiveCombination3XA (procedure, arg0, arg1, arg2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            NoteCalls (this.arg2);
            ratorHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            rand2TypeHistogram.Note (this.rand2Type);
            SCode.location = "PrimitiveCombination3XA";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3XA";
#endif
            if (ev2 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object ev0;
            env = environment;
            unev = this.arg0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };

#if DEBUG
            SCode.location = "PrimitiveCombination3XA";
#endif
            if (ev0 == Interpreter.UnwindStack) throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, environment.ArgumentValue(this.rand1Offset), ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class PrimitiveCombination3XA0 : PrimitiveCombination3XA
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand2TypeHistogram = new Histogram<Type> ();
#endif

        protected PrimitiveCombination3XA0 (Primitive3 procedure, SCode arg0, Argument0 arg1, SCode arg2)
            : base (procedure, arg0, arg1, arg2)
        {
        }

        public static SCode Make (Primitive3 procedure, SCode arg0, Argument0 arg1, SCode arg2)
        {
            return
                (arg2 is Argument) ? PrimitiveCombination3XA0A.Make (procedure, arg0, arg1, (Argument) arg2) :
                (arg2 is Quotation) ? new PrimitiveCombination3XA0Q (procedure, arg0, arg1, (Quotation) arg2) :
                new PrimitiveCombination3XA0 (procedure, arg0, arg1, arg2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            NoteCalls (this.arg2);
            ratorHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            rand2TypeHistogram.Note (this.rand2Type);
            SCode.location = "PrimitiveCombination3XA0";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3XA0";
#endif
            if (ev2 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object ev0;
            env = environment;
            unev = this.arg0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };

#if DEBUG
            SCode.location = "PrimitiveCombination3XA0";
#endif
            if (ev0 == Interpreter.UnwindStack) throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, environment.Argument0Value, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class PrimitiveCombination3XA0A : PrimitiveCombination3XA0
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand2Offset;

        protected PrimitiveCombination3XA0A (Primitive3 procedure, SCode arg0, Argument0 arg1, Argument arg2)
            : base (procedure, arg0, arg1, arg2)
        {
            this.rand2Offset = arg2.Offset;
        }

        public static SCode Make (Primitive3 procedure, SCode arg0, Argument0 arg1, Argument arg2)
        {
            return
                (arg2 is Argument0) ? new PrimitiveCombination3XA0A0 (procedure, arg0, arg1, (Argument0) arg2) :
                (arg2 is Argument1) ? new PrimitiveCombination3XA0A1 (procedure, arg0, arg1, (Argument1) arg2) :
                new PrimitiveCombination3XA0A (procedure, arg0, arg1, arg2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            ratorHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PrimitiveCombination3XA0A";
#endif
            object ev0;
            Environment env = environment;
            Control unev = this.arg0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };

#if DEBUG
            SCode.location = "PrimitiveCombination3XA0A";
#endif
            if (ev0 == Interpreter.UnwindStack) throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, environment.Argument0Value, environment.ArgumentValue(this.rand2Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3XA0A0 : PrimitiveCombination3XA0A
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif

        internal PrimitiveCombination3XA0A0 (Primitive3 procedure, SCode arg0, Argument0 arg1, Argument0 arg2)
            : base (procedure, arg0, arg1, arg2)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            ratorHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PrimitiveCombination3XA0A0";
#endif
            object ev0;
            Environment env = environment;
            Control unev = this.arg0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };

#if DEBUG
            SCode.location = "PrimitiveCombination3XA0A0";
#endif
            if (ev0 == Interpreter.UnwindStack) throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            object ev12 = environment.Argument0Value;
            if (this.method (out answer, ev0, ev12, ev12)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3XA0A1 : PrimitiveCombination3XA0A
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        internal PrimitiveCombination3XA0A1 (Primitive3 procedure, SCode arg0, Argument0 arg1, Argument1 arg2)
            : base (procedure, arg0, arg1, arg2)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            ratorHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PrimitiveCombination3XA0A1";
#endif
            object ev0;
            Environment env = environment;
            Control unev = this.arg0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };

#if DEBUG
            SCode.location = "PrimitiveCombination3XA0A1";
#endif
            if (ev0 == Interpreter.UnwindStack) throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, environment.Argument0Value, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3XA0Q : PrimitiveCombination3XA0
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand2Value;
        internal PrimitiveCombination3XA0Q (Primitive3 procedure, SCode arg0, Argument0 arg1, Quotation arg2)
            : base (procedure, arg0, arg1, arg2)
        {
            this.rand2Value = arg2.Quoted;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            ratorHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PrimitiveCombination3XA0Q";
#endif

            object ev0;
            Environment env = environment;
            Control unev = this.arg0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };

#if DEBUG
            SCode.location = "PrimitiveCombination3XA0Q";
#endif
            if (ev0 == Interpreter.UnwindStack) throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, environment.Argument0Value, this.rand2Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class PrimitiveCombination3XA1 : PrimitiveCombination3XA
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand2TypeHistogram = new Histogram<Type> ();
#endif

        protected PrimitiveCombination3XA1 (Primitive3 procedure, SCode arg0, Argument1 arg1, SCode arg2)
            : base (procedure, arg0, arg1, arg2)
        {
        }

        public static SCode Make (Primitive3 procedure, SCode arg0, Argument1 arg1, SCode arg2)
        {
            return
                (arg2 is Argument) ? PrimitiveCombination3XA1A.Make (procedure, arg0, arg1, (Argument) arg2) :
                new PrimitiveCombination3XA1 (procedure, arg0, arg1, arg2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            NoteCalls (this.arg2);
            ratorHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            rand2TypeHistogram.Note (this.rand2Type);
            SCode.location = "PrimitiveCombination3XA1";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3XA1";
#endif
            if (ev2 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object ev0;
            env = environment;
            unev = this.arg0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };

#if DEBUG
            SCode.location = "PrimitiveCombination3XA1";
#endif
            if (ev0 == Interpreter.UnwindStack) throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, environment.Argument1Value, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class PrimitiveCombination3XA1A : PrimitiveCombination3XA1
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand2Offset;
        protected PrimitiveCombination3XA1A (Primitive3 procedure, SCode arg0, Argument1 arg1, Argument arg2)
            : base (procedure, arg0, arg1, arg2)
        {
            this.rand2Offset = arg2.Offset;
        }

        public static SCode Make (Primitive3 procedure, SCode arg0, Argument1 arg1, Argument arg2)
        {
            return
                (arg2 is Argument0) ? new PrimitiveCombination3XA1A0 (procedure, arg0, arg1, (Argument0) arg2) :
                (arg2 is Argument1) ? new PrimitiveCombination3XA1A1 (procedure, arg0, arg1, (Argument1) arg2) :
                new PrimitiveCombination3XA1A (procedure, arg0, arg1, arg2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            ratorHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PrimitiveCombination3XA1A";
#endif

            object ev0;
            Environment env = environment;
            Control unev = this.arg0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };

#if DEBUG
            SCode.location = "PrimitiveCombination3XA1A";
#endif
            if (ev0 == Interpreter.UnwindStack) throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, environment.Argument1Value, environment.ArgumentValue(this.rand2Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3XA1A0 : PrimitiveCombination3XA1A
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        internal PrimitiveCombination3XA1A0 (Primitive3 procedure, SCode arg0, Argument1 arg1, Argument0 arg2)
            : base (procedure, arg0, arg1, arg2)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            ratorHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PrimitiveCombination3XA1A0";
#endif

            object ev0;
            Environment env = environment;
            Control unev = this.arg0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };

#if DEBUG
            SCode.location = "PrimitiveCombination3XA1A0";
#endif
            if (ev0 == Interpreter.UnwindStack) throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, environment.Argument1Value, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3XA1A1 : PrimitiveCombination3XA1A
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        internal PrimitiveCombination3XA1A1 (Primitive3 procedure, SCode arg0, Argument1 arg1, Argument1 arg2)
            : base (procedure, arg0, arg1, arg2)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            ratorHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PrimitiveCombination3XA1A1";
#endif

            object ev0;
            Environment env = environment;
            Control unev = this.arg0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };

#if DEBUG
            SCode.location = "PrimitiveCombination3XA1A1";
#endif
            if (ev0 == Interpreter.UnwindStack) throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, environment.Argument1Value, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class PrimitiveCombination3XQ : PrimitiveCombination3
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand2TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand1Value;

        protected PrimitiveCombination3XQ (Primitive3 procedure, SCode arg0, Quotation arg1, SCode arg2)
            : base (procedure, arg0, arg1, arg2)
        {
            this.rand1Value = arg1.Quoted;
        }

        public static SCode Make (Primitive3 procedure, SCode arg0, Quotation arg1, SCode arg2)
        {
            return
                (arg2 is Argument) ? PrimitiveCombination3XQA.Make (procedure, arg0, arg1, (Argument) arg2) :
                (arg2 is StaticVariable) ? new PrimitiveCombination3XQS (procedure, arg0, arg1, (StaticVariable) arg2) :
                (arg2 is Quotation) ? new PrimitiveCombination3XQQ (procedure, arg0, arg1, (Quotation) arg2) :
                new PrimitiveCombination3XQ (procedure, arg0, arg1, arg2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            NoteCalls (this.arg2);
            ratorHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            rand2TypeHistogram.Note (this.rand2Type);
            SCode.location = "PrimitiveCombination3XQ";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3XQ";
#endif
            if (ev2 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object ev0;
            env = environment;
            unev = this.arg0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };

#if DEBUG
            SCode.location = "PrimitiveCombination3XQ";
#endif
            if (ev0 == Interpreter.UnwindStack) throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, this.rand1Value, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
  }

    [Serializable]
    class PrimitiveCombination3XQA : PrimitiveCombination3XQ
    {

#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif

        public readonly int rand2Offset;

        protected PrimitiveCombination3XQA (Primitive3 procedure, SCode arg0, Quotation arg1, Argument rand2)
            : base (procedure, arg0, arg1, rand2)
        {
            this.rand2Offset = rand2.Offset;
        }

        public static SCode Make (Primitive3 procedure, SCode arg0, Quotation arg1, Argument rand2)
        {
            return
                (rand2 is Argument0) ? new PrimitiveCombination3XQA0 (procedure, arg0, arg1, (Argument0) rand2) :
                (rand2 is Argument1) ? new PrimitiveCombination3XQA1 (procedure, arg0, arg1, (Argument1) rand2) :
                new PrimitiveCombination3XQA (procedure, arg0, arg1, rand2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);

            ratorHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);

            SCode.location = "PrimitiveCombination3XQA";
#endif
            object ev2 = environment.ArgumentValue (this.rand2Offset);

            object ev0;
            Environment env = environment;
            Control unev = this.arg0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };

#if DEBUG
            SCode.location = "PrimitiveCombination3XQA";
#endif
            if (ev0 == Interpreter.UnwindStack) throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, this.rand1Value, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3XQA0 : PrimitiveCombination3XQA
    {

#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif

        internal PrimitiveCombination3XQA0 (Primitive3 procedure, SCode arg0, Quotation arg1, Argument0 rand2)
            : base (procedure, arg0, arg1, rand2)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);

            ratorHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);

            SCode.location = "PrimitiveCombination3XQA0";
#endif
            object ev2 = environment.Argument0Value;

            object ev0;
            Environment env = environment;
            Control unev = this.arg0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };

#if DEBUG
            SCode.location = "PrimitiveCombination3XQA0";
#endif
            if (ev0 == Interpreter.UnwindStack) throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, this.rand1Value, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3XQA1 : PrimitiveCombination3XQA
    {

#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif

        internal PrimitiveCombination3XQA1 (Primitive3 procedure, SCode arg0, Quotation arg1, Argument1 rand2)
            : base (procedure, arg0, arg1, rand2)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);

            ratorHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);

            SCode.location = "PrimitiveCombination3XQA1";
#endif
            object ev2 = environment.Argument1Value;

            object ev0;
            Environment env = environment;
            Control unev = this.arg0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };

#if DEBUG
            SCode.location = "PrimitiveCombination3XQA1";
#endif
            if (ev0 == Interpreter.UnwindStack) throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, this.rand1Value, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3XQS : PrimitiveCombination3XQ
    {

#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand2Name;
        public readonly int rand2Offset;

        internal PrimitiveCombination3XQS (Primitive3 procedure, SCode arg0, Quotation arg1, StaticVariable rand2)
            : base (procedure, arg0, arg1, rand2)
        {
            this.rand2Name = rand2.Name;
            this.rand2Offset = rand2.Offset;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);

            ratorHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);

            SCode.location = "PrimitiveCombination3XQS";
#endif
            object ev2;
            if (environment.StaticValue (out ev2, this.rand2Name, this.rand2Offset))
                throw new NotImplementedException ();

            object ev1;
            Environment env = environment;
            Control unev = this.arg1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3XQS";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame1 (this, environment, ev2));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object ev0;
            env = environment;
            unev = this.arg0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };

#if DEBUG
            SCode.location = "PrimitiveCombination3XQS";
#endif
            if (ev0 == Interpreter.UnwindStack) throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, ev1, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3XQQ : PrimitiveCombination3XQ
    {

#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        public readonly object rand2Value;

        internal PrimitiveCombination3XQQ (Primitive3 procedure, SCode arg0, Quotation arg1, Quotation rand2)
            : base (procedure, arg0, arg1, rand2)
        {
            this.rand2Value = rand2.Quoted;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            ratorHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PrimitiveCombination3XQQ";
#endif

            object ev0;
            Environment env = environment;
            Control unev = this.arg0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };

#if DEBUG
            SCode.location = "PrimitiveCombination3XQQ";
#endif
            if (ev0 == Interpreter.UnwindStack) throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, this.rand1Value, this.rand2Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class PrimitiveCombination3XS : PrimitiveCombination3
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand2TypeHistogram = new Histogram<Type> ();
#endif
        public readonly Symbol rand1Name;
        public readonly int rand1Offset;

        protected PrimitiveCombination3XS (Primitive3 procedure, SCode arg0, StaticVariable arg1, SCode arg2)
            : base (procedure, arg0, arg1, arg2)
        {
            this.rand1Name = arg1.Name;
            this.rand1Offset = arg1.Offset;
        }

        public static SCode Make (Primitive3 procedure, SCode arg0, StaticVariable arg1, SCode arg2)
        {
            return
                (arg2 is Argument) ? PrimitiveCombination3XSA.Make (procedure, arg0, arg1, (Argument) arg2) :
                new PrimitiveCombination3XS (procedure, arg0, arg1, arg2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            NoteCalls (this.arg2);
            ratorHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            rand2TypeHistogram.Note (this.rand2Type);
            SCode.location = "PrimitiveCombination3XS";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3XQ";
#endif
            if (ev2 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            env = environment;
            unev = this.arg0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };

#if DEBUG
            SCode.location = "PrimitiveCombination3XQ";
#endif
            if (ev0 == Interpreter.UnwindStack) throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, ev1, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class PrimitiveCombination3XSA : PrimitiveCombination3XS
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        public readonly int rand2Offset;

        protected PrimitiveCombination3XSA (Primitive3 procedure, SCode arg0, StaticVariable arg1, Argument arg2)
            : base (procedure, arg0, arg1, arg2)
        {
            this.rand2Offset = arg2.Offset;
        }

        public static SCode Make (Primitive3 procedure, SCode arg0, StaticVariable arg1, Argument arg2)
        {
            return
                (arg2 is Argument0) ? new PrimitiveCombination3XSA0 (procedure, arg0, arg1, (Argument0) arg2) :
                (arg2 is Argument1) ? new PrimitiveCombination3XSA1 (procedure, arg0, arg1, (Argument1) arg2) :
                new PrimitiveCombination3XSA (procedure, arg0, arg1, arg2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            ratorHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PrimitiveCombination3XSA";
#endif

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Environment env = environment;
            Control unev = this.arg0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };

#if DEBUG
            SCode.location = "PrimitiveCombination3XSA";
#endif
            if (ev0 == Interpreter.UnwindStack) throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, ev1, environment.ArgumentValue(this.rand2Offset))) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3XSA0 : PrimitiveCombination3XSA
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        internal PrimitiveCombination3XSA0 (Primitive3 procedure, SCode arg0, StaticVariable arg1, Argument0 arg2)
            : base (procedure, arg0, arg1, arg2)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            ratorHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PrimitiveCombination3XSA0";
#endif

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Environment env = environment;
            Control unev = this.arg0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };

#if DEBUG
            SCode.location = "PrimitiveCombination3XSA0";
#endif
            if (ev0 == Interpreter.UnwindStack) throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, ev1, environment.Argument0Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3XSA1 : PrimitiveCombination3XSA
    {
#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
#endif
        internal PrimitiveCombination3XSA1 (Primitive3 procedure, SCode arg0, StaticVariable arg1, Argument1 arg2)
            : base (procedure, arg0, arg1, arg2)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            ratorHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            SCode.location = "PrimitiveCombination3XSA1";
#endif

            object ev1;
            if (environment.StaticValue (out ev1, this.rand1Name, this.rand1Offset))
                throw new NotImplementedException ();

            object ev0;
            Environment env = environment;
            Control unev = this.arg0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };

#if DEBUG
            SCode.location = "PrimitiveCombination3XSA1";
#endif
            if (ev0 == Interpreter.UnwindStack) throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, ev1, environment.Argument1Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    class PrimitiveCombination3XXA : PrimitiveCombination3
    {

#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif

        public readonly int rand2Offset;

        protected PrimitiveCombination3XXA (Primitive3 procedure, SCode arg0, SCode arg1, Argument rand2)
            : base (procedure, arg0, arg1, rand2)
        {
            this.rand2Offset = rand2.Offset;
        }

        public static SCode Make (Primitive3 procedure, SCode arg0, SCode arg1, Argument rand2)
        {
            return
                (rand2 is Argument0) ? new PrimitiveCombination3XXA0 (procedure, arg0, arg1, (Argument0) rand2) :
                (rand2 is Argument1) ? new PrimitiveCombination3XXA1 (procedure, arg0, arg1, (Argument1) rand2) :
                new PrimitiveCombination3XXA (procedure, arg0, arg1, rand2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            NoteCalls (this.arg1);

            ratorHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);

            SCode.location = "PrimitiveCombination3XXA";
#endif
            object ev2 = environment.ArgumentValue (this.rand2Offset);

            object ev1;
            Environment env = environment;
            Control unev = this.arg1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3XXA";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame1 (this, environment, ev2));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object ev0;
            env = environment;
            unev = this.arg0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };

#if DEBUG
            SCode.location = "PrimitiveCombination3XXA";
#endif
            if (ev0 == Interpreter.UnwindStack) throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, ev1, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3XXA0 : PrimitiveCombination3XXA
    {

#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif

        internal PrimitiveCombination3XXA0 (Primitive3 procedure, SCode arg0, SCode arg1, Argument0 rand2)
            : base (procedure, arg0, arg1, rand2)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            NoteCalls (this.arg1);

            ratorHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);

            SCode.location = "PrimitiveCombination3XXA0";
#endif
            object ev2 = environment.Argument0Value;

            object ev1;
            Environment env = environment;
            Control unev = this.arg1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3XXA0";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame1 (this, environment, ev2));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object ev0;
            env = environment;
            unev = this.arg0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };

#if DEBUG
            SCode.location = "PrimitiveCombination3XXA0";
#endif
            if (ev0 == Interpreter.UnwindStack) throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, ev1, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3XXA1 : PrimitiveCombination3XXA
    {

#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();
#endif

        internal PrimitiveCombination3XXA1 (Primitive3 procedure, SCode arg0, SCode arg1, Argument1 rand2)
            : base (procedure, arg0, arg1, rand2)
        {
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            NoteCalls (this.arg1);

            ratorHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);

            SCode.location = "PrimitiveCombination3XXA1";
#endif
            object ev2 = environment.Argument1Value;

            object ev1;
            Environment env = environment;
            Control unev = this.arg1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3XXA1";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame1 (this, environment, ev2));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object ev0;
            env = environment;
            unev = this.arg0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };

#if DEBUG
            SCode.location = "PrimitiveCombination3XXA1";
#endif
            if (ev0 == Interpreter.UnwindStack) throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, ev1, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3XXS : PrimitiveCombination3
    {

#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();

#endif
        public readonly Symbol rand2Name;
        public readonly int rand2Offset;

        internal PrimitiveCombination3XXS (Primitive3 procedure, SCode arg0, SCode arg1, StaticVariable rand2)
            : base (procedure, arg0, arg1, rand2)
        {
            this.rand2Name = rand2.Name;
            this.rand2Offset = rand2.Offset;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            NoteCalls (this.arg1);

            ratorHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);

            SCode.location = "PrimitiveCombination3XXS";
#endif
            object ev2;
            if (environment.StaticValue (out ev2, this.rand2Name, this.rand2Offset))
                throw new NotImplementedException ();

            object ev1;
            Environment env = environment;
            Control unev = this.arg1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3XXS";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame1 (this, environment, ev2));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object ev0;
            env = environment;
            unev = this.arg0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };

#if DEBUG
            SCode.location = "PrimitiveCombination3XXS";
#endif
            if (ev0 == Interpreter.UnwindStack) throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, ev1, ev2)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3XXQ : PrimitiveCombination3
    {

#if DEBUG
        static Histogram<Primitive3> ratorHistogram = new Histogram<Primitive3> ();
        static Histogram<Type> rand0TypeHistogram = new Histogram<Type> ();
        static Histogram<Type> rand1TypeHistogram = new Histogram<Type> ();

#endif
        public readonly object rand2Value;

        internal PrimitiveCombination3XXQ (Primitive3 procedure, SCode arg0, SCode arg1, Quotation rand2)
            : base (procedure, arg0, arg1, rand2)
        {
            this.rand2Value = rand2.Quoted;
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            NoteCalls (this.arg0);
            NoteCalls (this.arg1);
            ratorHistogram.Note (this.procedure);
            rand0TypeHistogram.Note (this.rand0Type);
            rand1TypeHistogram.Note (this.rand1Type);
            SCode.location = "PrimitiveCombination3XXQ";
#endif

            object ev1;
            Environment env = environment;
            Control unev = this.arg1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3XXQ";
#endif
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination3Frame1 (this, environment, this.rand2Value));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object ev0;
            env = environment;
            unev = this.arg0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };

#if DEBUG
            SCode.location = "PrimitiveCombination3XXQ";
#endif
            if (ev0 == Interpreter.UnwindStack) throw new NotImplementedException ();

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString ();
#endif
            if (this.method (out answer, ev0, ev1, this.rand2Value)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci != null) {
                    expression = tci.Expression;
                    environment = tci.Environment;
                    answer = null;
                    return true;
                }
                throw new NotImplementedException ();
            }
            return false;
        }
    }
}
