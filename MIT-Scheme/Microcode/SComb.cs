using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Runtime.Serialization;

namespace Microcode
{
    [Serializable]
    class Combination : SCode, ISystemVector
    {
#if DEBUG
        static Histogram<int> combinationSizeHistogram = new Histogram<int>();
#endif

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode [] components;

        protected Combination (SCode [] components)
            : base (TC.COMBINATION)
        {
            object oper = components [0];
            this.components = components;
        }

        Combination (object [] components)
            : base (TC.COMBINATION)
        {
            SCode[] comps = new SCode [components.Length];
            for (int i = 0; i < components.Length; i++)
                comps [i] = EnsureSCode (components [i]);
            this.components = comps;
        }

        Combination(Cons components)
            : base (TC.COMBINATION)
        {
            SCode[] comps = new SCode[components.Length()];
            int i = 0;
            while (components != null)
            {
                SCode component = EnsureSCode (components.Car);
                comps[i] = component;
                components = (Cons)(components.Cdr);
                i += 1;
            }
            this.components = comps;
        }

        public static SCode Make (object [] components)
        {
            object oper = components [0];

            if (Configuration.EnableSuperOperators) {
                switch (components.Length) {
                    case 0:
                        throw new NotImplementedException ("shouldn't be possible");

                    case 1:
                        //Debugger.Break ();
                        return Combination0.Make (oper);

                    case 2:
                        throw new NotImplementedException ("combo 1");

                    case 3:
                        throw new NotImplementedException ("combo 2");

                    case 4:
                        return Combination3.Make (EnsureSCode (oper), EnsureSCode (components [1]), EnsureSCode (components [2]), EnsureSCode (components [3]));

                    case 5:
                        return Combination4.Make (EnsureSCode (oper), EnsureSCode (components [1]), EnsureSCode (components [2]), EnsureSCode (components [3]), EnsureSCode (components [4]));

                    default:
                        return new Combination (components);
                }
            }
            else
                return new Combination (components);
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
                return this.components[0];
            }
        }

        [SchemePrimitive ("COMBINATION?", 1, true)]
        public static bool IsCombination (out object answer, object arg)
        {
            answer = arg is Combination || arg is Combination0;
            return false;
        }

         public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            bool anyChange = false;
            SCode [] opt = new SCode [this.components.Length];
            for (int i = 0; i < this.components.Length; i++) {
                opt [i] = this.components [i].Bind (ctenv);
                anyChange = anyChange || (opt [i] != this.components [i]);
            }
            return anyChange
                ? Combination.Make (opt)
                : this;
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
            Warm ();
            combinationSizeHistogram.Note (this.components.Length);
#endif
            object rator = null;
            object [] evaluated = new object [this.components.Length - 1];
            int counter = this.components.Length - 1;
            while (counter > -1) {
                Control expr = components [counter];
#if DEBUG
                noteCalls ((SCode)expr);
#endif
                Environment env = environment;
                object ev;
                while (expr.EvalStep (out ev, ref expr, ref env)) { };
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
                    return Interpreter.Call (out answer, ref expression, ref environment, rator, evaluated[0]);
                case 2:
                    Debugger.Break ();
                    return Interpreter.Call (out answer, ref expression, ref environment, rator, evaluated [0], evaluated[1]);
                case 3:
                    return Interpreter.Call (out answer, ref expression, ref environment, rator, evaluated [0], evaluated [1], evaluated[2]);
                case 4:
                    return Interpreter.Call (out answer, ref expression, ref environment, rator, evaluated [0], evaluated [1], evaluated [2], evaluated[3]);
                default:
                    return Interpreter.Apply (out answer, ref expression, ref environment, rator, evaluated);
            }
        }

        internal static object FromList (Cons cons)
        {
            return Combination.Make (cons.ToVector ());
        }

        public override bool MutatesAny (object [] formals)
        {
            foreach (SCode component in this.components)
                if (component.MutatesAny (formals))
                    return true;
            return false;
        }

        public override bool UsesAny (object [] formals)
        {
            foreach (SCode component in this.components)
                if (component.UsesAny (formals))
                    return true;
            return false;
        }

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
            int counter = this.counter;
            evaluated [counter--] = value;

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
                evaluated [counter--] = ev;
            }

            return Interpreter.Apply (out answer, ref expression, ref environment, evaluated [0], evaluated);

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
    sealed class Combination0 : SCode, ISystemVector
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode rator;

        Combination0 (SCode rator)
            : base (TC.COMBINATION)
        {
            if (rator == null)
                throw new ArgumentNullException ("rator");
            this.rator = rator;
        }

        Combination0 (object rator)
            : base (TC.COMBINATION)
        {
            this.rator = EnsureSCode (rator);
        }

        public static SCode Make (object rator)
        {
            //if (Configuration.EnableStaticEnvironments) {
            //    Lambda lrator = rator as Lambda;
            //    if (lrator == null) {
            //        ExtendedLambda elrator = rator as ExtendedLambda;
            //        if (elrator == null) {
            //        }
            //        else 
            //            if (elrator.required == 0
            //                && elrator.optional == 0
            //                && elrator.rest == false)
            //                return elrator.Body;
                    
            //    }
            //    else {
            //        Debugger.Break ();
            //    }
            //}

            return new Combination0 (rator);
        }

        public SCode Operator
        {
            [DebuggerStepThrough]
            get
            {
                return this.rator;
            }
        }

        public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            SCode optimizedRator = this.rator.Bind (ctenv);
            return optimizedRator == this.rator 
                ? this
                : new Combination0 (optimizedRator);
        }

        public override bool CallsTheEnvironment ()
        {
            return this.rator.CallsTheEnvironment ();
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.rator);
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

        public override bool MutatesAny (object [] formals)
        {
            return this.rator.MutatesAny (formals);
        }

        public override bool UsesAny (object [] formals)
        {
            return this.rator.UsesAny (formals);
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
    class Combination3 : Combination
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode rator;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode rand0;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode rand1;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode rand2;

        protected Combination3 (SCode rator, SCode rand0, SCode rand1, SCode rand2)
            : base (new SCode [] {rator, rand0, rand1, rand2})
        {
            this.rator = rator;
            this.rand0 = rand0;
            this.rand1 = rand1;
            this.rand2 = rand2;
        }

        public static SCode Make (SCode rator, SCode rand0, SCode rand1, SCode rand2)
        {
            return new Combination3 (rator, rand0, rand1, rand2);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Warm ();
            noteCalls (this.rator);
            noteCalls (this.rand0);
            noteCalls (this.rand1);
            noteCalls (this.rand2);
#endif
            object ev2 = null;
            Environment env = environment;
            Control unev2 = this.rand2;
            while (unev2.EvalStep (out ev2, ref unev2, ref env)) { };
            if (ev2 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                //environment = env;
                //answer = Interpreter.UnwindStack;
                //return false;
            }

            object ev1 = null;
            env = environment;
            Control unev1 = this.rand1;
            while (unev1.EvalStep (out ev1, ref unev1, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                //environment = env;
                //answer = Interpreter.UnwindStack;
                //return false;
            }

            object ev0 = null;
            env = environment;
            Control unev0 = this.rand0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
                //environment = env;
                //answer = Interpreter.UnwindStack;
                //return false;
            }

            object evop = null;
            env = environment;
            Control unevop = this.rator;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
                //((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
                //environment = env;
                //answer = Interpreter.UnwindStack;
                //return false;
            }

            // expression = (SCode) evop;
            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1, ev2);
        }

    }

    [Serializable]
    class Combination4 : Combination
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode rator;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode rand0;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode rand1;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode rand2;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        protected readonly SCode rand3;

               protected Combination4 (SCode rator, SCode rand0, SCode rand1, SCode rand2, SCode rand3)
            : base (new SCode [] {rator, rand0, rand1, rand2, rand3})
        {
            this.rator = rator;
            this.rand0 = rand0;
            this.rand1 = rand1;
            this.rand2 = rand2;
                   this.rand3 = rand3;
        }

               public static SCode Make (SCode rator, SCode rand0, SCode rand1, SCode rand2, SCode rand3)
               {
                   return new Combination4 (rator, rand0, rand1, rand2, rand3);
               }

               public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
               {
#if DEBUG
                   Warm ();
                   noteCalls (this.rator);
                   noteCalls (this.rand0);
                   noteCalls (this.rand1);
                   noteCalls (this.rand2);
                   noteCalls (this.rand3);

#endif
                   object ev3 = null;
                   Environment env = environment;
                   Control unev3 = this.rand3;
                   while (unev3.EvalStep (out ev3, ref unev3, ref env)) { };
                   if (ev3 == Interpreter.UnwindStack) {
                       throw new NotImplementedException ();
                       //((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                       //environment = env;
                       //answer = Interpreter.UnwindStack;
                       //return false;
                   }
                   object ev2 = null;
                   env = environment;
                   Control unev2 = this.rand2;
                   while (unev2.EvalStep (out ev2, ref unev2, ref env)) { };
                   if (ev2 == Interpreter.UnwindStack) {
                       throw new NotImplementedException ();
                       //((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                       //environment = env;
                       //answer = Interpreter.UnwindStack;
                       //return false;
                   }

                   object ev1 = null;
                   env = environment;
                   Control unev1 = this.rand1;
                   while (unev1.EvalStep (out ev1, ref unev1, ref env)) { };
                   if (ev1 == Interpreter.UnwindStack) {
                       throw new NotImplementedException ();
                       //((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                       //environment = env;
                       //answer = Interpreter.UnwindStack;
                       //return false;
                   }

                   object ev0 = null;
                   env = environment;
                   Control unev0 = this.rand0;
                   while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
                   if (ev0 == Interpreter.UnwindStack) {
                       throw new NotImplementedException ();
                       //((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
                       //environment = env;
                       //answer = Interpreter.UnwindStack;
                       //return false;
                   }

                   object evop = null;
                   env = environment;
                   Control unevop = this.rator;
                   while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
                   if (evop == Interpreter.UnwindStack) {
                       throw new NotImplementedException ();
                       //((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
                       //environment = env;
                       //answer = Interpreter.UnwindStack;
                       //return false;
                   }

                   // expression = (SCode) evop;
                   return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1, ev2, ev3);
               }

    }

    [Serializable]
    sealed class PrimitiveCombination0 : SCode
    {

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly Primitive0 procedure;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        [NonSerialized]
        PrimitiveMethod0 method;

        public PrimitiveCombination0 (Primitive0 procedure)
            : base (TC.PCOMB0)
        {
            if (procedure == null) throw new ArgumentNullException ("procedure");
            this.procedure = procedure;
            this.method = procedure.Method;
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

        public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            return this;
        }

        public override bool CallsTheEnvironment ()
        {
            return false;
        }

        public override bool EvalStep (out object answer, ref Control expression,ref Environment environment)
        {
#if DEBUG
            Warm ();
#endif 
            if (this.method (out answer)) {
                throw new NotImplementedException ();
            }
            else return false;
        }

        public override bool MutatesAny (object [] formals)
        {
            return false;
        }

        public override bool UsesAny (object [] formals)
        {
            return false;
        }
    }
    
    [Serializable]
    sealed class PrimitiveCombination3 : SCode
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly Primitive3 procedure;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        [NonSerialized]
        PrimitiveMethod3 method;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode arg0;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode arg1;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode arg2;

        public PrimitiveCombination3 (Primitive3 procedure, object arg0, object arg1, object arg2)
            : base (TC.PCOMB3)
        {
            if (procedure == null) throw new ArgumentNullException ("procedure");
            this.procedure = procedure;
            this.method = procedure.Method;
            this.arg0 = EnsureSCode (arg0);
            this.arg1 = EnsureSCode (arg1);
            this.arg2 = EnsureSCode (arg2);
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

        public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            SCode o0 = this.Operand0.Bind (ctenv);
            SCode o1 = this.Operand1.Bind (ctenv);
            SCode o2 = this.Operand2.Bind (ctenv);
            if (o0 == this.Operand0
                && o1 == this.Operand1
                && o2 == this.Operand2)
                return this;
            else
            return new PrimitiveCombination3 (this.procedure,
                o0, o1, o2);
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
            Warm ();
            noteCalls (this.arg0);
            noteCalls (this.arg1);
            noteCalls (this.arg2);
#endif
            object ev2 = null;
            Environment env = environment;
            Control unev = this.arg2;
            while (unev.EvalStep (out ev2, ref unev, ref env)) { };
            if (ev2 == Interpreter.UnwindStack) throw new NotImplementedException ();

            object ev1 = null;
            env = environment;
            unev = this.arg1;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) throw new NotImplementedException ();

            object ev0 = null;
            env = environment;
            unev = this.arg0;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) throw new NotImplementedException ();

            // It may be expensive to bounce down to invoke the procedure
            // we should invoke it directly and pass along the ref args.
            // Calling directly may break tail recursion for primitives
            // that call back.
#if DEBUG
            Primitive.hotPrimitives.Note (this.procedure);
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

        public override bool MutatesAny (object [] formals)
        {
            return this.arg0.MutatesAny (formals)
                || this.arg1.MutatesAny (formals)
                || this.arg2.MutatesAny (formals);
        }

        public override bool UsesAny (object [] formals)
        {
            return this.arg0.UsesAny (formals)
                || this.arg1.UsesAny (formals)
                || this.arg2.UsesAny (formals);
        }
    }
}
