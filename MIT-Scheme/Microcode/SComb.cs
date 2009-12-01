using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Runtime.Serialization;
using System.Security.Permissions;

namespace Microcode
{
    [Serializable]
    class Combination : SCode, ISerializable, ISystemVector
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public override TC TypeCode { get { return TC.COMBINATION; } }

#if DEBUG
        static Histogram<int> combinationSizeHistogram = new Histogram<int> ();
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode [] components;

        protected Combination (SCode [] components)
            : base ()
        {
            this.components = components;
        }

        Combination (object [] components)
            : base ()
        {
            SCode[] comps = new SCode [components.Length];
            for (int i = 0; i < components.Length; i++)
                comps [i] = EnsureSCode (components [i]);
            this.components = comps;
        }

        Combination (Cons components)
            : base ()
        {
            SCode[] comps = new SCode [components.Length ()];
            int j = 0;
            while (components != null) {
                SCode component = EnsureSCode (components.Car);
                comps [j] = component;
                components = (Cons) (components.Cdr);
                j += 1;
            }
            this.components = comps;
        }

        public static SCode Make (object [] components)
        {
            return
                (!Configuration.EnableCombinationOptimization) ? new Combination(components) :
                (Configuration.EnableCombination0 && components.Length == 1) ? Combination0.Make(components[0]) :
                (Configuration.EnableCombination3 && components.Length == 4) ? Combination3.Make(EnsureSCode(components[0]), EnsureSCode(components[1]), EnsureSCode(components[2]), EnsureSCode(components[3])):
                new Combination(components);

            //if (Configuration.EnableSuperOperators) {
            //    switch (components.Length) {
            //        case 0:
            //            throw new NotImplementedException ("shouldn't be possible");

            //        //case 1:
            //        //    //Debugger.Break ();
            //        //    return Combination0.Make (oper);

            //        case 2:
            //            throw new NotImplementedException ("combo 1");

            //        case 3:
            //            throw new NotImplementedException ("combo 2");

            //        case 4:
            //            return Configuration.EnableCombination3 ?
            //                   Combination3.Make (EnsureSCode (oper),
            //                                      EnsureSCode (components [1]),
            //                                      EnsureSCode (components [2]),
            //                                      EnsureSCode (components [3])) :
            //                  new Combination (components);

            //        default:
            //            return new Combination (components);
            //    }
            //}
            //else
            //    return new Combination (components);
        }

        public SCode [] Components
        {
            [DebuggerStepThrough]
            get
            {
                return this.components;
            }
        }

        public SCode Operator
        {
            [DebuggerStepThrough]
            get
            {
                return this.components [0];
            }
        }

        [SchemePrimitive ("COMBINATION?", 1, true)]
        public static bool IsCombination (out object answer, object arg)
        {
            answer = arg is Combination || arg is Combination0;//|| arg is Combination3 || arg is Combination4;
            return false;
        }

        public override bool CallsTheEnvironment ()
        {
            foreach (SCode component in this.components)
                if (component.CallsTheEnvironment ())
                    return true;
            return false;
        }

#if DEBUG
        public override string Key ()
        {
            string answer = "(combination ";
            foreach (SCode component in components)
                answer += component.Key ();
            return answer + ")";
        }
#endif

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            combinationSizeHistogram.Note (this.components.Length);
            SCode.location = "Combination.EvalStep";
#endif
            object rator = null;
            object [] evaluated = new object [this.components.Length - 1];
            int counter = this.components.Length - 1;
            while (counter > -1) {
                object ev;
                Environment env = environment;
                Control expr = components [counter];
#if DEBUG
                NoteCalls ((SCode) expr);
#endif
                while (expr.EvalStep (out ev, ref expr, ref env)) { };
#if DEBUG
                SCode.location = "Combination.EvalStep";
#endif
                if (ev == Interpreter.UnwindStack) {
                    ((UnwinderState) env).AddFrame (new CombinationFrame (this, environment, evaluated, counter));
                    environment = env;
                    answer = Interpreter.UnwindStack;
                    return false;
                }
                if (counter == 0) {
                    rator = ev;
                    break;
                }
                else
                    evaluated [--counter] = ev;
            }
            switch (evaluated.Length) {
                case 0:
                    return Interpreter.Call (out answer, ref expression, ref environment, rator);
                case 1:
                    Debugger.Break ();
                    return Interpreter.Call (out answer, ref expression, ref environment, rator, evaluated [0]);
                case 2:
                    Debugger.Break ();
                    return Interpreter.Call (out answer, ref expression, ref environment, rator, evaluated [0], evaluated [1]);
                case 3:
                    return Interpreter.Call (out answer, ref expression, ref environment, rator, evaluated [0], evaluated [1], evaluated [2]);
                default:
                    return Interpreter.Apply (out answer, ref expression, ref environment, rator, evaluated);
            }
        }

        internal static object FromList (Cons cons)
        {
            return Combination.Make (cons.ToVector ());
        }

        public override bool MutatesAny (Symbol [] formals)
        {
            foreach (SCode component in this.components)
                if (component.MutatesAny (formals))
                    return true;
            return false;
        }

        #region ISerializable Members

        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        public void GetObjectData (SerializationInfo info, StreamingContext context)
        {
            info.SetType (typeof (CombinationDeserializer));
            info.AddValue ("combinationSize", this.components.Length);
            for (int i = 0; i < this.components.Length; i++)
                info.AddValue ("component_" + i, this.components [i]);
        }

        #endregion


        #region ISystemVector Members
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public int SystemVectorSize
        {
            get { return this.components.Length; }
        }

        public object SystemVectorRef (int index)
        {
            return UnwrapQuoted (this.components [index]);
        }

        public object SystemVectorSet (int index, object newValue)
        {
            throw new NotImplementedException ();
        }

        #endregion

        internal override PartialResult PartialEval (PartialEnvironment environment)
        {
            bool changed = false;
            PartialResult [] ra = new PartialResult [this.components.Length];
            for (int i = 0; i < this.components.Length; i++) {
                ra [i] = this.components [i].PartialEval (environment);
                if (ra [i].Residual != this.components [i]) changed = true;
            }
            if (changed) {
                SCode [] components = new SCode [ra.Length];
                for (int i = 0; i < ra.Length; i++) {
                    components [i] = ra [i].Residual;
                }
                return new PartialResult (new Combination (components));
            }
            else {
                return new PartialResult (this);
            }
        }

        public override void CollectFreeVariables (HashSet<Symbol> freeVariableSet)
        {
            foreach (SCode component in this.components)
                component.CollectFreeVariables (freeVariableSet);
        }
    }

    [Serializable]
    sealed class CombinationDeserializer : ISerializable, IObjectReference
    {
        [NonSerialized]
        SCode [] components;

        [NonSerialized]
        SCode realObject;

        CombinationDeserializer (SerializationInfo info, StreamingContext context)
        {
            uint size = info.GetUInt32 ("combinationSize"); 
            this.components = new SCode [size];
            for (int i = 0; i < size; i++)
                components [i] = (SCode) info.GetValue ("component_" + i, typeof(SCode));
        }

        #region ISerializable Members

        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        void ISerializable.GetObjectData (SerializationInfo info, StreamingContext context)
        {
            throw new NotImplementedException ();
        }
        #endregion

        #region IObjectReference Members

        public object GetRealObject (StreamingContext context)
        {
            if (this.realObject == null)
                this.realObject = Combination.Make (components);
            return this.realObject;
        }
        #endregion
    }

    [Serializable]
    sealed class CombinationFrame : SubproblemContinuation<Combination>, ISystemVector
    {
        object [] evaluated;
        int counter;

        public CombinationFrame (Combination combination, Environment environment, object [] evaluated, int counter)
            :base (combination, environment)
        {
            this.evaluated = (object []) (evaluated.Clone());
            this.counter = counter;
        }

        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object value)
        {
            object [] evaluated = this.evaluated;
            object rator = null;
            int counter = this.counter;
            if (counter == 0) {
                rator = value;
            }
            else {
                evaluated [--counter] = value;

                while (counter > -1) {
                    Control expr = this.expression.Components [counter];
                    Environment env = environment;
                    object ev = null;
                    while (expr.EvalStep (out ev, ref expr, ref env)) { };
                    if (ev == Interpreter.UnwindStack) {
                        ((UnwinderState) env).AddFrame (new CombinationFrame (this.expression, environment, evaluated, counter));
                        environment = env;
                        answer = Interpreter.UnwindStack;
                        return false;
                    }
                    if (counter == 0) {
                        rator = ev;
                        break;
                    }
                    else
                        evaluated [--counter] = ev;
                }
            }

            return Interpreter.Apply (out answer, ref expression, ref environment, rator, evaluated);

         }

        #region ISystemVector Members

        public int SystemVectorSize
        {
            get { return this.evaluated.Length + 4; }
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
    class Combination0 : SCode, ISystemVector
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public override TC TypeCode { get { return TC.COMBINATION; } }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode rator;

        protected Combination0 (SCode rator)
            : base ()
        {
            if (rator == null)
                throw new ArgumentNullException ("rator");
            this.rator = rator;
        }

        Combination0 (object rator)
            : base ()
        {
            this.rator = EnsureSCode (rator);
        }

        //static SCode SpecialMake (StaticLambda rator)
        //{
        //    // Can you believe this happens too often?!
        //   // Debug.WriteLine ("; Optimize (let () <lambdaBody>) => <lambdaBody>");
        //    Debug.Write (".");
        //    return rator.Body;
        //}

        public static SCode Make (object rator)
        {
            return 
                //(! Configuration.EnableCombination0Optimization) ? new Combination0 (rator) :
                ////// Combination of no arguments simply applied, just insert the lambdaBody.
                ////// This confuses the pretty printer, though.
                //////: (rator is StaticLambda && ((Lambda) rator).Formals.Length == 0) ? SpecialMake ((StaticLambda) rator)
                ////: (rator is StaticLambda && ((Lambda) rator).Formals.Length == 0) ? SpecialCombination0.Make ((StaticLambda) rator)
                ////: 
                new Combination0 (rator);
        }

        public SCode Operator
        {
            [DebuggerStepThrough]
            get
            {
                return this.rator;
            }
        }

        public override bool CallsTheEnvironment ()
        {
            return this.rator.CallsTheEnvironment ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination0.EvalStep");
            NoteCalls (this.rator);
#endif
            object evop = null;
            Control unevop = this.rator;
            Environment env = environment;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination0Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop);
        }

        public override bool MutatesAny (Symbol [] formals)
        {
            return this.rator.MutatesAny (formals);
        }

        #region ISystemVector Members

        public int SystemVectorSize
        {
            get { return 1; }
        }

        public object SystemVectorRef (int index)
        {
            if (index == 0)
                return this.rator;
            else
                throw new NotImplementedException ();
        }

        public object SystemVectorSet (int index, object newValue)
        {
            throw new NotImplementedException ();
        }

        #endregion

        internal override PartialResult PartialEval (PartialEnvironment environment)
        {
            PartialResult po = this.Operator.PartialEval (environment);
            return new PartialResult ((po.Residual == this.Operator) ? this : new Combination0 (po.Residual));
        }

        public override void CollectFreeVariables (HashSet<Symbol> freeVariableSet)
        {
            this.Operator.CollectFreeVariables (freeVariableSet);
        }
    }

    [Serializable]
    sealed class Combination0Frame0 : SubproblemContinuation<Combination0>, ISystemVector
    {

        public Combination0Frame0 (Combination0 combination0, Environment environment)
            : base (combination0, environment)
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
            return Interpreter.Call (out answer, ref expression, ref environment, value);
        }
    }

//    class SpecialCombination0 : Combination0
//    {
//        Symbol name;
//        Symbol [] formals;
//        SCode body;

//        //protected SpecialCombination0 (StaticLambda rator)
//        //    : base (rator)
//        //{
//        //    this.name = rator.Name;
//        //    this.formals = rator.Formals;
//        //    this.body = rator.Body;
//        //}

//        public static SCode Make (StaticLambda rator)
//        {
//            return new SpecialCombination0 (rator);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
//        {
//#if DEBUG
//            Warm ("SpecialCombination0.EvalStep");
//#endif
//            expression = this.body;
//            answer = null;
//            return true;
//        }
//    }


//    [Serializable]
//    class Combination0A : Combination0
//    {
//        protected Combination0A (Argument rator)
//            : base (rator)
//        {
//        }

//        public static SCode Make (Argument rator)
//        {
//            return (rator is Argument0) ? Combination0A0.Make ((Argument0)rator)
//                : (rator is Argument1) ? Combination0A1.Make ((Argument1)rator)
//                : new Combination0A (rator);
//        }
        
//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination0A.EvalStep");
//#endif
//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, closureEnvironment.ArgumentValue (this.ratorAddress.Offset));
//        }
//    }

//    [Serializable]
//    class Combination0A0 : Combination0A
//    {
//        protected Combination0A0 (Argument0 rator)
//            : base (rator)
//        {
//        }

//        public static SCode Make (Argument0 rator)
//        {
//            return  new Combination0A0 (rator);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination0A0.EvalStep");
//#endif
//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, closureEnvironment.Argument0Value);
//        }
//    }

//    [Serializable]
//    class Combination0A1 : Combination0A
//    {
//        protected Combination0A1 (Argument1 rator)
//            : base (rator)
//        {
//        }

//        public static SCode Make (Argument1 rator)
//        {
//            return new Combination0A1 (rator);
//        }

//        public override bool EvalStep (out object answer, ref Control expression, ref Environment closureEnvironment)
//        {
//#if DEBUG
//            Warm ("Combination0A1.EvalStep");
//#endif
//            return Interpreter.Call (out answer, ref expression, ref closureEnvironment, closureEnvironment.Argument1Value);
//        }
//    }

    [Serializable]
    sealed class PrimitiveCombination0 : SCode, ISerializable
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public override TC TypeCode { get { return TC.PCOMB0; } }


        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly Primitive0 procedure;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        [NonSerialized]
        PrimitiveMethod0 method;

        PrimitiveCombination0 (Primitive0 procedure)
            : base ()
        {
            this.procedure = procedure;
            this.method = procedure.Method;
        }

        public static SCode Make (Primitive0 procedure)
        {
            if (procedure == null) throw new ArgumentNullException ("procedure");
            return new PrimitiveCombination0 (procedure);
        }

        [OnDeserialized ()]
        internal void OnDeserializedMethod (StreamingContext context)
        {
            this.method = procedure.Method;
        } 

        public Primitive0 Operator
        {
            [DebuggerStepThrough]
            get
            {
                return this.procedure;
            }
        }

        [SchemePrimitive ("PRIMITIVE-COMBINATION0?", 1, true)]
        public static bool IsPrimitiveCombination0 (out object answer, object arg)
        {
            answer = arg is PrimitiveCombination0;
            return false;
        }

        public override bool CallsTheEnvironment ()
        {
            return false;
        }

        public override bool EvalStep (out object answer, ref Control expression,ref Environment environment)
        {
#if DEBUG
            Warm ("-");
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString();
#endif 
            if (this.method (out answer)) {
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
            return false;
        }

        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        void ISerializable.GetObjectData (SerializationInfo info, StreamingContext context)
        {
            info.SetType (typeof (PrimitiveCombination0Deserializer));
            info.AddValue ("procedure", this.procedure);
        }

        internal override PartialResult PartialEval (PartialEnvironment environment)
        {
            return new PartialResult (this);
        }

        public override void CollectFreeVariables (HashSet<Symbol> freeVariableSet)
        {
            return;
        }
    }

    [Serializable]
    internal sealed class PrimitiveCombination0Deserializer : IObjectReference
    {
        Primitive0 procedure;

        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        public Object GetRealObject (StreamingContext context)
        {
            return PrimitiveCombination0.Make (this.procedure);
        }

        Primitive0 Procedure { set { this.procedure = value; } }
    }

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
                new PrimitiveCombination3 (procedure, arg0, arg1, arg2);
        }

        public static SCode Make (object procedure, object arg0, object arg1, object arg2)
        {
            return
                PrimitiveCombination3.Make ((Primitive3) procedure, EnsureSCode(arg0), EnsureSCode(arg1), EnsureSCode(arg2));
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
            SCode.location = "PrimitiveCombination3.EvalStep";
#endif
            object ev2;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
#if DEBUG
            SCode.location = "PrimitiveCombination3.EvalStep";
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
            SCode.location = "PrimitiveCombination3.EvalStep";
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
            SCode.location = "PrimitiveCombination3.EvalStep.3";
#endif
            if (ev0 == Interpreter.UnwindStack) throw new NotImplementedException (); 

#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
            SCode.location = this.procedure.Name.ToString();
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
            SCode.location = this.expression.Operator.Name.ToString();
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

}
