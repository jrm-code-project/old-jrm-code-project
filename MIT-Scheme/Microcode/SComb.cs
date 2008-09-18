using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Runtime.Serialization;

namespace Microcode
{
    [Serializable]
    sealed class Combination : SCode, ISystemVector
    {
#if DEBUG
        static long creationCount;
        static long evaluationCount;
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode [] components;

        Combination (SCode [] components)
            : base (TC.COMBINATION)
        {
#if DEBUG
            Combination.creationCount += 1;
#endif
            object oper = components [0];
            //if (oper is Lambda
            //    || oper is ExtendedLambda
            //    || oper is Primitive)
            //    Debugger.Break ();
            this.components = components;
        }

        Combination (object [] components)
            : base (TC.COMBINATION)
        {
#if DEBUG
            Combination.creationCount += 1;
#endif

            SCode[] comps = new SCode [components.Length];
            for (int i = 0; i < components.Length; i++)
                comps [i] = EnsureSCode (components [i]);
            this.components = comps;
        }

        Combination(Cons components)
            : base (TC.COMBINATION)
        {
#if DEBUG
            Combination.creationCount += 1;
#endif

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
            //if (oper is Lambda
            //    || oper is ExtendedLambda
            //    || oper is Primitive
            //    || oper is Quotation)
            //    Debugger.Break ();

            switch (components.Length) {
                case 0:
                    throw new NotImplementedException ("shouldn't be possible");

                case 1:
                    //Debugger.Break ();
                    return Combination0.Make (oper);

                case 2:
                    throw new NotImplementedException ("combo 1");

                default:
                    return new Combination (components);
            }
        }

        public SCode [] Components
        {
            [DebuggerStepThrough]
            get
            {
                return this.components;
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
                ? new Combination (opt)
                : this;
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
            Combination.evaluationCount += 1;
            Warm ();
#endif
            object rator = null;
            object [] evaluated = new object [this.components.Length - 1];
            int counter = this.components.Length - 1;
            while (counter > -1) {
                Control expr = components [counter];   
                Environment env = environment;
                object ev = null;
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
                    Debugger.Break ();
                    return Interpreter.Call (out answer, ref expression, ref environment, rator);
                case 1:
                    Debugger.Break ();
                    return Interpreter.Call (out answer, ref expression, ref environment, rator, evaluated[0]);
                case 2:
                    Debugger.Break ();
                    return Interpreter.Call (out answer, ref expression, ref environment, rator, evaluated [0], evaluated[1]);
                case 3:
                    //Debugger.Break ();
                    return Interpreter.Call (out answer, ref expression, ref environment, rator, evaluated [0], evaluated [1], evaluated[2]);
                default:
                    return Interpreter.Apply (out answer, ref expression, ref environment, rator, evaluated);
            }
        }

        public override IList<object> FreeVariables ()
        {
            IList<object> answer = new List<object> ();
            foreach (SCode component in this.components)
                foreach (object var in component.FreeVariables ())
                    if (!answer.Contains (var)) answer.Add (var);
            return answer;
        }

        internal static object FromList (Cons cons)
        {
            return Combination.Make (cons.ToVector ());
        }

        public override bool NeedsValueCells (object [] formals)
        {
            foreach (SCode component in this.components)
                if (component.NeedsValueCells (formals))
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
    sealed class Combination0 : SCode, ISystemVector
    {
#if DEBUG
        static long creationCount;
        static long evaluationCount;
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode rator;

        Combination0 (SCode rator)
            : base (TC.COMBINATION)
        {
#if DEBUG
            Combination0.creationCount += 1;
#endif

            if (rator == null)
                throw new ArgumentNullException ("rator");
            this.rator = rator;
        }

        Combination0 (object rator)
            : base (TC.COMBINATION)
        {
#if DEBUG
            Combination0.creationCount += 1;
#endif

            this.rator = EnsureSCode (rator);
        }

        public static SCode Make (object rator)
        {
            if (rator is Lambda
               // || rator is ExtendedLambda
                 )
                Debugger.Break ();
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

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Combination0.evaluationCount += 1;
            Warm ();
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

        public override IList<object> FreeVariables ()
        {
            return this.rator.FreeVariables ();
        }

        public override bool NeedsValueCells (object [] formals)
        {
            return this.rator.NeedsValueCells (formals);
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

#if DEBUG

        public override string Key ()
        {
            return "(comb0 " + this.rator.Key () + ")";
        }
#endif
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
    sealed class Combination1 : SCode, ISystemPair
    {
#if DEBUG
        static long creationCount;
        static long evaluationCount;
        static long literalOpCount;
        static long variableCount;
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode rator;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode rand;

        Combination1 (SCode rator, SCode rand)
            :base (TC.COMBINATION_1)
        {
#if DEBUG
            Combination1.creationCount += 1;
#endif

            if (rator == null)
                throw new ArgumentNullException ("rator");
            if (rand == null)
                throw new ArgumentNullException ("rand");
            this.rator = rator;
            this.rand = rand;
        }

        Combination1 (object rator, object rand)
            : base (TC.COMBINATION_1)
        {
#if DEBUG
            Combination1.creationCount += 1;
#endif
            if (rator is SimpleLambda
    && ((SimpleLambda) rator).Formals.Length != 1)
                throw new NotImplementedException ("making a combo1 with wrong operator");

            this.rator = EnsureSCode (rator);
            this.rand = EnsureSCode (rand);
        }

        public static SCode Make (object rator, object arg0) 
        {
            if (rator is Primitive
                && ((Primitive) rator).Arity != -1)
                Debugger.Break ();
            if (rator is Quotation) Debugger.Break ();


            //if (rator is Lambda
            //    || rator is ExtendedLambda
            //    || rator is Primitive
            //    || rator is Quotation)
            //    Debugger.Break ();
            return new Combination1 (rator, arg0);
        }

        public SCode Operator
        {
            [DebuggerStepThrough]
            get
            {
                return this.rator;
            }
        }

        public object Operand
        {
            [DebuggerStepThrough]
            get
            {
                return UnwrapQuoted(this.rand);
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

                return UnwrapQuoted(this.rator);
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

                return UnwrapQuoted(this.rand);
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        #endregion

        public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            SCode optimizedRator = this.rator.Bind (ctenv);
            SCode optimizedRand = this.rand.Bind (ctenv);
            return optimizedRator == this.rator && optimizedRand == this.rand
                ? this
                : new Combination1 (optimizedRator, optimizedRand);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Combination1.evaluationCount += 1;
            if (this.rator is Lambda
                || this.rator is ExtendedLambda)
                Combination1.literalOpCount += 1;
            else if (this.rator is Variable)
                Combination1.variableCount += 1;
            Warm ();
#endif

            object evarg = null;
            Control unev = this.rand;
            Environment env = environment;
            while (unev.EvalStep (out evarg, ref unev, ref env)) { };
            if (evarg == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination1Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object evop = null;
            Control unevop = this.rator;
            env = environment;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
            if (evop == Interpreter.UnwindStack) { 
                ((UnwinderState) env).AddFrame (new Combination1Frame1 (this, environment, evarg));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            } 

            return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg);
        }

        public override IList<object> FreeVariables ()
        {
            IList<object> answer = new List<object> ();
            foreach (object var in rator.FreeVariables ())
                if (!answer.Contains (var)) answer.Add (var);
            foreach (object var in rand.FreeVariables ())
                if (!answer.Contains (var)) answer.Add (var);
            return answer;
        }

        public override bool NeedsValueCells (object [] formals)
        {
            return this.rator.NeedsValueCells (formals)
                || this.rand.NeedsValueCells (formals);
        }
#if DEBUG
        public override string Key ()
        {
            return "Comb1-" + this.serialNumber.ToString ();
        }
#endif
    }

    [Serializable]
    sealed class Combination1Frame0 : SubproblemContinuation<Combination1>, ISystemVector
    {

        public Combination1Frame0 (Combination1 combination1, Environment environment)
            :base (combination1, environment)
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
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    class Combination2 : SCode, ISystemHunk3
    {
#if DEBUG
        static long creationCount;
        static long evaluationCount;
        static long literalOpCount;
        static long variableCount;
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode rator;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode rand0;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode rand1;

        public Combination2 (object rator, object rand0, object rand1)
            : base (TC.COMBINATION_2)
        {
#if DEBUG
            Combination2.creationCount += 1;
#endif

            this.rator = EnsureSCode (rator);
            this.rand0 = EnsureSCode (rand0);
            this.rand1 = EnsureSCode (rand1);
        }

        public Combination2 (Hunk3 init)
            : base (TC.COMBINATION_2)
        {
#if DEBUG
            Combination2.creationCount += 1;
#endif

            this.rator = EnsureSCode (init.Cxr0);
            this.rand0 = EnsureSCode (init.Cxr1);
            this.rand1 = EnsureSCode (init.Cxr2);
        }

        public static SCode Make (object rator, object rand0, object rand1)
        {
            return new Combination2 (rator, rand0, rand1);
        }

        public SCode Rand0
        {
            [DebuggerStepThrough]
            get
            {
                return this.rand0;
            }
        }

        public SCode Rand1
        {
            [DebuggerStepThrough]
            get
            {
                return this.rand1;
            }
        }

        public SCode Rator
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

        public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            return new Combination2 (this.rator.Bind (ctenv),
                                     this.rand0.Bind (ctenv),
                                     this.rand1.Bind (ctenv));
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Combination2.evaluationCount += 1;
            if (this.rator is Lambda
                || this.rator is ExtendedLambda)
                Combination2.literalOpCount += 1;
            else if (this.rator is Variable)
                Combination2.variableCount += 1;
            Warm ();
#endif

            object ev1 = null;
            Environment env = environment;
            Control unev1 = this.rand1;
            while (unev1.EvalStep (out ev1, ref unev1, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object ev0 = null;
            env = environment;
            Control unev0 = this.rand0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame1 (this, environment, ev1));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object evop = null;
            env = environment;
            Control unevop = this.rator;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame2 (this, environment, ev0, ev1));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            // expression = (SCode) evop;
            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }

        public override IList<object> FreeVariables ()
        {
            IList<object> answer = new List<object> ();
            foreach (object var in this.rator.FreeVariables ())
                if (!answer.Contains (var)) answer.Add (var);
            foreach (object var in this.rand0.FreeVariables ())
                if (!answer.Contains (var)) answer.Add (var);
            foreach (object var in this.rand1.FreeVariables ())
                if (!answer.Contains (var)) answer.Add (var);
            return answer;
        }

        public override bool NeedsValueCells (object [] formals)
        {
            return this.rator.NeedsValueCells (formals)
                || this.rand0.NeedsValueCells (formals)
                || this.rand1.NeedsValueCells (formals);
        }
#if DEBUG
        public override string Key ()
        {
            return "Comb2-" + this.serialNumber.ToString ();
        }
#endif
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
            Control unev0 = this.expression.Rand0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            object evop = null;
            env = environment;
            Control unevop = this.expression.Rator;
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
            Control unevop = this.expression.Rator;
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
            Control unevop = this.expression.Rator;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, value, this.ev0, this.ev1);
        }
    }

    [Serializable]
    sealed class PrimitiveCombination0 : SCode
    {
#if DEBUG
        static long evaluationCount;
#endif

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

        public override bool EvalStep (out object answer, ref Control expression,ref Environment environment)
        {
#if DEBUG
            PrimitiveCombination0.evaluationCount += 1;
            Warm ();
#endif 
            if (this.method (out answer)) {
                throw new NotImplementedException ();
            }
            else return false;
        }

        public override IList<object> FreeVariables ()
        {
            return new List<object> ();
        }

        public override bool NeedsValueCells (object [] formals)
        {
            return false;
        }
#if DEBUG
        public override string Key ()
        {
            return "(pcomb0 " + this.procedure.ToString () + ")";
        }
#endif
    }
    
    [Serializable]
    sealed class PrimitiveCombination1 : SCode
    {
#if DEBUG
        static long evaluationCount;
#endif
 
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly Primitive1 procedure;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        [NonSerialized]
        PrimitiveMethod1 method;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode arg0;

        PrimitiveCombination1 (Primitive1 procedure,  SCode arg0)
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

        public static SCode Make (Primitive1 rator, object rand)
        {
            if (rator == null) throw new ArgumentNullException ("rator");
            if (rand is Argument0) return new PrimitiveCombination1A (rator);
            SCode srand = EnsureSCode (rand);
            SCode answer;
#if DEBUG  
            string key =  "(pc1 " + rator.ToString () + " " + srand.Key () + ")";
            if (SCode.hashConsTable.TryGetValue (key, out answer)) return answer;
#endif
            answer = new PrimitiveCombination1 (rator, srand);
#if DEBUG
            SCode.hashConsTable.Add (key, answer);
#endif
            return answer;
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

        public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            SCode optArg0 = this.arg0.Bind (ctenv);
            return optArg0 == this.arg0
                ? this
                : PrimitiveCombination1.Make (this.procedure, optArg0);
        }

        public override bool EvalStep (out object answer, ref Control expression,ref Environment environment)
        {
#if DEBUG
            PrimitiveCombination1.evaluationCount += 1;
            Warm ();
#endif
            Control unev0 = this.arg0;
            Environment env = environment;
            object ev0 = null;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination1Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            this.procedure.invocationCount += 1;
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

        public override IList<object> FreeVariables ()
        {
            IList<object> answer = new List<object> ();
            foreach (object var in this.arg0.FreeVariables ())
                if (!answer.Contains (var)) answer.Add (var);
            return answer;

        }

        public override bool NeedsValueCells (object [] formals)
        {
            return this.arg0.NeedsValueCells (formals);
        }
#if DEBUG
        public override string Key ()
        {
            return "(pc1 " + this.procedure.ToString() + " " + arg0.Key () + ")";
        }
#endif
    }

    [Serializable]
    sealed class PrimitiveCombination1Frame0 : SubproblemContinuation<PrimitiveCombination1>, ISystemVector
    {

        public PrimitiveCombination1Frame0 (PrimitiveCombination1 expression, Environment environment)
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

        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object value)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    sealed class PrimitiveCombination1A : SCode
    {
#if DEBUG
        static long evaluationCount;
#endif

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly Primitive1 procedure;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        [NonSerialized]
        PrimitiveMethod1 method;

        public PrimitiveCombination1A (Primitive1 procedure)
            : base (TC.PCOMB1)
        {
            this.procedure = procedure;
            this.method = procedure.Method;
        }

        [OnDeserialized ()]
        internal void OnDeserializedMethod (StreamingContext context)
        {
            this.method = procedure.Method;
        }

        public Primitive1 Operator
        {
            [DebuggerStepThrough]
            get
            {
                return this.procedure;
            }
        }

        //public SCode Operand
        //{
        //    [DebuggerStepThrough]
        //    get
        //    {
        //        return this.arg0;
        //    }
        //}

        public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            return this;
            //SCode optArg0 = this.arg0.Bind (ctenv);
            //return optArg0 == this.arg0
            //    ? this
            //    : new PrimitiveCombination1A (this.procedure, optArg0);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            PrimitiveCombination1A.evaluationCount += 1;
            Warm ();
#endif

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            this.procedure.invocationCount += 1;
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

        public override IList<object> FreeVariables ()
        {
            throw new NotImplementedException ();
        }


        public override bool NeedsValueCells (object [] formals)
        {
            return false;
        }
#if DEBUG
        public override string Key ()
        {
            return "(pcomb1a " + this.procedure.ToString () + ")";
        }
#endif
    }

    [Serializable]
    sealed class PrimitiveCombination1AFrame0 : SubproblemContinuation<PrimitiveCombination1A>, ISystemVector
    {

        public PrimitiveCombination1AFrame0 (PrimitiveCombination1A expression, Environment environment)
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

        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object value)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    sealed class PrimitiveCombination2 : SCode, ISystemHunk3
    {
#if DEBUG
        [NonSerialized]
        static long evaluationCount;
#endif

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly Primitive2 rator;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        [NonSerialized]
        PrimitiveMethod2 method;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode rand0;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode rand1;

        PrimitiveCombination2 (Primitive2 rator, SCode rand0, SCode rand1)
            : base (TC.PCOMB2)
        {
            this.rator = rator;
            this.method = rator.Method;
            this.rand0 = rand0;
            this.rand1 = rand1;
        }

        [OnDeserialized ()]
        internal void OnDeserializedMethod (StreamingContext context)
        {
            this.method = rator.Method;
        } 

        public Primitive2 Rator
        {
            [DebuggerStepThrough]
            get
            {
                return this.rator;
            }
        }

        public SCode Rand0
        {
            [DebuggerStepThrough]
            get
            {
                return this.rand0;
            }
        }

        public SCode Rand1
        {
            [DebuggerStepThrough]
            get
            {
                return this.rand1;
            }
        }

        public static SCode Make (Primitive2 rator, object rand0, object rand1)
        {
            if (rator == null)
                throw new ArgumentNullException ("rator");

            //if (rand0 is int
            //    || rand1 is int)
            //    Debugger.Break ();


            //if (rator == Primitive.Find ("EQ?", 2))
            //    return new PrimitiveEq (rand0, rand1);
            if (rator == Primitive.Find ("MINUS-FIXNUM", 2)
                && rand1 is int
                && (int) rand1 == 1) {
                return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("MINUS-ONE-PLUS-FIXNUM", 1), rand0);
            }

            // fixup calls to object-type?
            else if ((rator == Primitive.Find ("PRIMITIVE-OBJECT-TYPE?", 2)
                || rator == Primitive.Find ("OBJECT-TYPE?", 2))
                && rand0 is int) {
                TC code = (TC) (int) rand0;
                switch (code) {
                    case TC.ACCESS:
                        return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("ACCESS?", 1), rand1);
                    case TC.ASSIGNMENT:
                        return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("ASSIGNMENT?", 1), rand1);
                    case TC.BIG_FIXNUM:
                        return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("BIG-FIXNUM?", 1), rand1);
                    case TC.BIG_FLONUM:
                        return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("BIG-FLONUM?", 1), rand1);
                    case TC.BROKEN_HEART:
                        return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("BROKEN-HEART?", 1), rand1);
                    case TC.COMBINATION:
                        return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("COMBINATION?", 1), rand1);
                    case TC.COMBINATION_1:
                        return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("COMBINATION1?", 1), rand1);
                    case TC.COMBINATION_2:
                        return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("COMBINATION2?", 1), rand1);
                    case TC.COMPILED_CODE_BLOCK:
                        return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("COMPILED-CODE-BLOCK?", 1), rand1);
                    case TC.COMPILED_ENTRY:
                        return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("COMPILED-ENTRY?", 1), rand1);
                    case TC.COMPLEX:
                        return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("COMPLEX?", 1), rand1);
                    case TC.COMMENT:
                        return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("COMMENT?", 1), rand1);
                    case TC.CONDITIONAL:
                        return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("CONDITIONAL?", 1), rand1);
                    case TC.CONTROL_POINT:
                        return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("CONTROL-POINT?", 1), rand1);
                    case TC.DEFINITION:
                        return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("DEFINITION?", 1), rand1);
                    case TC.DELAY:
                        return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("DELAY?", 1), rand1);
                    case TC.DELAYED:
                        return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("DELAYED?", 1), rand1);
                    case TC.DISJUNCTION:
                        return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("DISJUNCTION?", 1), rand1);
                    case TC.ENTITY:
                        return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("ENTITY?", 1), rand1);
                    case TC.ENVIRONMENT:
                        return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("ENVIRONMENT?", 1), rand1);
                    case TC.EXTENDED_LAMBDA:
                        return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("EXTENDED-LAMBDA?", 1), rand1);
                    case TC.EXTENDED_PROCEDURE:
                        return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("EXTENDED-PROCEDURE?", 1), rand1);
                    case TC.FIXNUM:
                        return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("FIXNUM?", 1), rand1);
                    case TC.HUNK3_B:
                        return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("HUNK3-B?", 1), rand1);
                    case TC.INTERNED_SYMBOL:
                        return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("SYMBOL?", 1), rand1);
                    case TC.LAMBDA:
                        return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("LAMBDA?", 1), rand1);
                    case TC.LEXPR:
                        return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("LEXPR?", 1), rand1);
                    case TC.MANIFEST_CLOSURE:
                        return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("MANIFEST-CLOSURE?", 1), rand1);
                    case TC.MANIFEST_NM_VECTOR:
                        return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("MANIFEST-NM-VECTOR?", 1), rand1);
                    case TC.PCOMB0:
                        return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("PRIMITIVE-COMBINATION0?", 1), rand1);
                    case TC.PCOMB1:
                        return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("PRIMITIVE-COMBINATION1?", 1), rand1);
                    case TC.PCOMB2:
                        return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("PRIMITIVE-COMBINATION2?", 1), rand1);
                    case TC.PCOMB3:
                        return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("PRIMITIVE-COMBINATION3?", 1), rand1);
                    case TC.PRIMITIVE:
                        return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("PRIMITIVE?", 1), rand1);
                    case TC.PROCEDURE:
                        return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("PROCEDURE?", 1), rand1);
                    case TC.RATNUM:
                        return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("RATNUM?", 1), rand1);
                    case TC.REFERENCE_TRAP:
                        return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("REFERENCE-TRAP?", 1), rand1);
                    case TC.RETURN_CODE:
                        return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("RETURN-CODE?", 1), rand1);
                    case TC.SCODE_QUOTE:
                        return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("SCODE-QUOTE?", 1), rand1);
                    case TC.SEQUENCE_2:
                        return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("SEQUENCE2?", 1), rand1);
                    case TC.SEQUENCE_3:
                        return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("SEQUENCE3?", 1), rand1);
                    case TC.STACK_ENVIRONMENT:
                        return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("STACK-ENVIRONMENT?", 1), rand1);
                    case TC.THE_ENVIRONMENT:
                        return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("THE-ENVIRONMENT?", 1), rand1);
                    case TC.UNINTERNED_SYMBOL:
                        return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("UNINTERNED-SYMBOL?", 1), rand1);
                    case TC.VARIABLE:
                        return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("VARIABLE?", 1), rand1);
                    case TC.VECTOR:
                        return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("VECTOR?", 1), rand1);
                    case TC.WEAK_CONS:
                        return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("WEAK-CONS?", 1), rand1);
                    default:
                        throw new NotImplementedException ();
                }
            }
            else if (rator == Primitive.Find ("PLUS-FIXNUM", 2)
                && rand1 is int
                && (int) rand1 == 1) {
                return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("ONE-PLUS-FIXNUM", 1), rand0);
            }
            else if (rator == Primitive.Find ("%RECORD-REF", 2)
                && rand1 is int
                && (int) rand1 == 1) {
                return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("%RECORD-REF1", 1), rand0);
            }
            else if (rator == Primitive.Find ("%RECORD-REF", 2)
                && rand1 is int
                && (int) rand1 == 3) {
                return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("%RECORD-REF3", 1), rand0);
            }
            else if (rator == Primitive.Find ("VECTOR-REF", 2)
                && rand1 is int
                && (int) rand1 == 0) {
                return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("VECTOR-REF0", 1), rand0);
            }
            else if (rator == Primitive.Find ("VECTOR-REF", 2)
                && rand1 is int
                && (int) rand1 == 1) {
                return PrimitiveCombination1.Make ((Primitive1) Primitive.Find ("VECTOR-REF1", 1), rand0);
            }

            else {
                //if (rand1 is int && (int) rand1 == 1) Debugger.Break ();
                SCode srand0 = EnsureSCode (rand0);
                SCode srand1 = EnsureSCode (rand1);
                SCode answer;     
#if DEBUG
                string key = "(pc2 " + rator.ToString () + " " + srand0.Key () + " " + srand1.Key () + ")";

                if (SCode.hashConsTable.TryGetValue (key, out answer)) return answer;
#endif
                answer = new PrimitiveCombination2 (rator, srand0, srand1);
#if DEBUG
                SCode.hashConsTable.Add (key, answer);
#endif
                return answer;
            }
        }

        public static SCode Make (Hunk3 init)
        {
            //Primitive2 rator = (Primitive2) init.Cxr0;
            //if (rator == Primitive.Find ("EQ?", 2))
            //    return new PrimitiveEq (init.Cxr1, init.Cxr2);
            return Make ((Primitive2) init.Cxr0, init.Cxr1, init.Cxr2);
        }

        [SchemePrimitive ("PRIMITIVE-COMBINATION2?", 1, true)]
        public static bool IsPrimitiveCombination2 (out object answer, object arg)
        {
            answer = arg is PrimitiveCombination2;
            return false;
        }

         public override SCode Bind (BindingTimeEnvironment ctenv)
        {
            SCode optRand0 = this.rand0.Bind (ctenv);
            SCode optRand1 = this.rand1.Bind (ctenv);
            return optRand0 == this.rand0
                && optRand1 == this.rand1
                ? this
                : new PrimitiveCombination2 (this.rator, optRand0, optRand1);
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            PrimitiveCombination2.evaluationCount += 1;
            Warm ();
#endif
            Control unev = this.rand1;
            Environment env = environment;
            object ev1 = null;
            while (unev.EvalStep (out ev1, ref unev, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            unev = this.rand0;
            env = environment;
            object ev0 = null;
            while (unev.EvalStep (out ev0, ref unev, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new PrimitiveCombination2Frame1 (this, environment, ev1));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.
#if DEBUG
            this.rator.invocationCount += 1;
#endif
            if (this.method (out answer, ev0, ev1)) {
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

        public override IList<object> FreeVariables ()
        {
            IList<object> answer = new List<object> ();
            foreach (object var in this.rand0.FreeVariables ())
                if (!answer.Contains (var)) answer.Add (var);
            foreach (object var in this.rand1.FreeVariables ())
                if (!answer.Contains (var)) answer.Add (var);
            return answer;
        }

        public override bool NeedsValueCells (object [] formals)
        {
            return this.rand0.NeedsValueCells (formals)
                || this.rand1.NeedsValueCells (formals);
        }

        #region ISystemHunk3 Members
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemHunk3Cxr0
        {
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
        public object SystemHunk3Cxr1
        {
            get
            {
                return UnwrapQuoted (this.rand0);
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
                return UnwrapQuoted (this.rand1);
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        #endregion

#if DEBUG
        public override string Key ()
        {
           return  "(pc2 " + this.rator.ToString() + " " + this.rand0.Key () + " " + this.rand1.Key () + ")"; 
        }
#endif
    }

    [Serializable]
    sealed class PrimitiveCombination2Frame0 : SubproblemContinuation<PrimitiveCombination2>, ISystemVector
    {

        public PrimitiveCombination2Frame0 (PrimitiveCombination2 expression, Environment environment)
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

        public override bool Continue (out object answer, ref Control expression, ref Environment environment, object value)
        {
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    sealed class PrimitiveCombination2Frame1 : SubproblemContinuation<PrimitiveCombination2>, ISystemVector
    {
        readonly object ev1;

        internal PrimitiveCombination2Frame1 (PrimitiveCombination2 expression, Environment environment, object ev1)
            : base (expression, environment)
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
            throw new NotImplementedException ();
        }
    }

    [Serializable]
    sealed class PrimitiveCombination3 : SCode
    {
#if DEBUG
        [NonSerialized]
        static long evaluationCount;
#endif
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
            return new PrimitiveCombination3 (this.procedure,
                this.Operand0.Bind (ctenv),
                this.Operand1.Bind (ctenv),
                this.Operand2.Bind (ctenv));
        }

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            PrimitiveCombination3.evaluationCount += 1;
            Warm ();
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
            this.procedure.invocationCount += 1;
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

        public override IList<object> FreeVariables ()
        {
            IList<object> answer = new List<object> ();
            foreach (object var in this.arg0.FreeVariables ())
                if (!answer.Contains (var)) answer.Add (var);
            foreach (object var in this.arg1.FreeVariables ())
                if (!answer.Contains (var)) answer.Add (var);
            foreach (object var in this.arg2.FreeVariables ())
                if (!answer.Contains (var)) answer.Add (var);
            return answer;
        }

        public override bool NeedsValueCells (object [] formals)
        {
            return this.arg0.NeedsValueCells (formals)
                || this.arg1.NeedsValueCells (formals)
                || this.arg2.NeedsValueCells (formals);
        }
#if DEBUG
        public override string Key ()
        {
            return "(pc3 "
                + this.procedure.ToString ()
                + " "
                + this.arg0.Key ()
                + " "
                + this.arg1.Key ()
                + " "
                + this.arg2.Key ()
                + ")";
        }
#endif
    }
}
