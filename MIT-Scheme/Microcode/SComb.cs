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
            SCode.location = "Combination";
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
                SCode.location = "Combination";
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
            : this (EnsureSCode (rator))
        {
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
            return Make (EnsureSCode (rator));
        }

        public static SCode Make (SCode rator)
        {
            return
                //(! Configuration.EnableCombination0Optimization) ? new Combination0 (rator) :
                ////// Combination of no arguments simply applied, just insert the lambdaBody.
                ////// This confuses the pretty printer, though.
                //////: (rator is StaticLambda && ((Lambda) rator).Formals.Length == 0) ? SpecialMake ((StaticLambda) rator)
                ////: (rator is StaticLambda && ((Lambda) rator).Formals.Length == 0) ? SpecialCombination0.Make ((StaticLambda) rator)
                ////: 
                (rator is Argument) ? Combination0A.Make ((Argument) rator) :
                (rator is StaticVariable) ? Combination0S.Make ((StaticVariable) rator) :
                (rator is Quotation) ? Combination0Q.Make ((Quotation) rator) :
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
            Warm ("Combination0");
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
            return new PartialResult ((po.Residual == this.Operator) ? this : Combination0.Make (po.Residual));
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

    [Serializable]
    class Combination0A : Combination0
    {
        public readonly int ratorOffset;
        protected Combination0A (Argument rator)
            : base (rator)
        {
            this.ratorOffset = rator.Offset;
        }

        public static SCode Make (Argument rator)
        {
            return
                (rator is Argument0) ? Combination0A0.Make ((Argument0) rator) :
                (rator is Argument1) ? Combination0A1.Make ((Argument1) rator) :
                new Combination0A (rator);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination0A");
#endif
            return Interpreter.Call (out answer, ref expression, ref environment, environment.ArgumentValue (this.ratorOffset));
        }
    }

    [Serializable]
    class Combination0A0 : Combination0A
    {
        Combination0A0 (Argument0 rator)
            : base (rator)
        {
        }

        public static SCode Make (Argument0 rator)
        {
            return
                new Combination0A0 (rator);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination0A0");
#endif
            return Interpreter.Call (out answer, ref expression, ref environment, environment.Argument0Value);
        }
    }

    [Serializable]
    class Combination0A1 : Combination0A
    {
        Combination0A1 (Argument1 rator)
            : base (rator)
        {
        }

        public static SCode Make (Argument1 rator)
        {
            return
                new Combination0A1 (rator);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination0A1");
#endif
            return Interpreter.Call (out answer, ref expression, ref environment, environment.Argument1Value);
        }
    }

    [Serializable]
    sealed class Combination0S : Combination0
    {
        public readonly Symbol ratorName;
        public readonly int ratorOffset;
        Combination0S (StaticVariable rator)
            : base (rator)
        {
            this.ratorName = rator.Name;
           this.ratorOffset = rator.Offset;
        }

        public static SCode Make (StaticVariable rator)
        {
            return
                new Combination0S (rator);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination0S");
#endif
            object evop;
            if (environment.StaticValue (out evop, this.ratorName, this.ratorOffset))
                throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop);
        }
    }

    [Serializable]
    sealed class Combination0Q : Combination0
    {
        public readonly IApplicable ratorValue;
        Combination0Q (Quotation rator)
            : base (rator)
        {
            this.ratorValue = (IApplicable) rator.Quoted;
        }

        public static SCode Make (Quotation rator)
        {
            return
                new Combination0Q (rator);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ("Combination0Q");
#endif
            return this.ratorValue.Call (out answer, ref expression, ref environment);
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
//            Warm ("SpecialCombination0");
//#endif
//            expression = this.body;
//            answer = null;
//            return true;
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
}
