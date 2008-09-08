using System;
using System.Collections.Generic;
using System.Diagnostics;

namespace Microcode
{
    sealed class Combination : SCode, ISystemVector
    {
#if DEBUG
        static long evaluationCount;
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode [] components;

        public Combination (SCode [] components)
            : base (TC.COMBINATION)
        {
            this.components = components;
        }

        public Combination (object [] components)
            : base (TC.COMBINATION)
        {
            SCode[] comps = new SCode [components.Length];
            for (int i = 0; i < components.Length; i++)
                comps [i] = EnsureSCode (components [i]);
            this.components = comps;
        }

        public Combination(Cons components)
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

        public SCode [] Components
        {
            [DebuggerStepThrough]
            get
            {
                return this.components;
            }
        }

        public override SCode Bind (CompileTimeEnvironment ctenv)
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

        public override bool EvalStep (out object answer, ref SCode expression, ref Environment environment)
        {
#if DEBUG
            Combination.evaluationCount += 1;
#endif
            object [] evaluated = new object [this.components.Length];
            int counter = this.components.Length - 1;
            while (counter > -1) {
                SCode expr = components [counter];   
                Environment env = environment;
                object ev = null;
                while (expr.EvalStep (out ev, ref expr, ref env)) { };
                if (ev == Interpreter.UnwindStack) {
                    ((UnwinderState) env).AddFrame (new CombinationFrame (this, environment, evaluated, counter));
                    environment = env;
                    answer = Interpreter.UnwindStack;
                    return false;
                } 
                evaluated [counter--] = ev;
            }

            return Interpreter.Apply (out answer, ref expression, ref environment, evaluated [0], evaluated);
        }

        public override HashSet<string> FreeVariables ()
        {
            HashSet<string> freeVariables = new HashSet<string>();
            foreach (SCode component in components)
                freeVariables.UnionWith (component.FreeVariables ());
            return freeVariables;
        }

        public override bool NeedsTheEnvironment ()
        {
            foreach (SCode component in this.components)
                if (component.NeedsTheEnvironment ())
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

        public override bool Continue (out object answer, ref SCode expression, ref Environment environment, object value)
        {
            object [] evaluated = this.evaluated;
            int counter = this.counter;
            evaluated [counter--] = value;

            while (counter > -1) {
                SCode expr = this.expression.Components [counter];
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

    //sealed class CombinationAccumulate : Continuation
    //{
    //    [DebuggerBrowsable (DebuggerBrowsableState.Never)]
    //    readonly SCode [] components;

    //    [DebuggerBrowsable (DebuggerBrowsableState.Never)]
    //    readonly Cons values;

    //    [DebuggerBrowsable (DebuggerBrowsableState.Never)]
    //    int index;

    //    [DebuggerBrowsable (DebuggerBrowsableState.Never)]
    //    readonly Environment environment;

    //    public CombinationAccumulate (Continuation next, SCode [] components, Cons values, int index, Environment environment)
    //        : base (next)
    //    {
    //        this.components = components;
    //        this.values = values;
    //        this.index = index;
    //        this.environment = environment;
    //    }

    //    //internal override object Invoke (Interpreter interpreter, object value)
    //    //{
    //    //    if (this.index == 1)
    //    //    {
    //    //        object [] valuevector = new object [this.components.Length - 1];
    //    //        Cons tail = this.values;
    //    //        int scan = 1;
    //    //        valuevector [0] = value;
    //    //        while (tail != null)
    //    //        {
    //    //            valuevector [scan++] = tail.Car;
    //    //            tail = (Cons) tail.Cdr;
    //    //        }
    //    //        return interpreter.EvalReuseSubproblem (this.components [0], this.environment, new CombinationApply (this.Parent, valuevector));
    //    //    }
    //    //    else
    //    //        return interpreter.EvalReuseSubproblem (this.components [this.index - 1], this.environment,
    //    //                                           new CombinationAccumulate (this.Parent,
    //    //                                                                      this.components,
    //    //                                                                      new Cons (value, this.values),
    //    //                                                                      this.index - 1,
    //    //                                                                      this.environment));
    //    //}

    //    public override int FrameSize
    //    {
    //        get { return 3 + this.components.Length; }
    //    }
    //}

    //sealed class CombinationApply : Continuation
    //{
    //    [DebuggerBrowsable (DebuggerBrowsableState.Never)]
    //    readonly object [] arguments;

    //    public CombinationApply (Continuation next, object [] arguments)
    //        : base (next)
    //    {
    //        this.arguments = arguments;
    //    }

    //    internal override object Invoke (Interpreter interpreter, object value)
    //    {
    //        return interpreter.Apply (value, arguments);
    //    }

    //    public override int FrameSize
    //    {
    //        get { throw new NotImplementedException (); }
    //    }
    //}


    sealed class Combination1 : SCode, ISystemPair
    {
#if DEBUG
        static long evaluationCount;
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode rator;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode rand;

        public Combination1 (SCode rator, SCode rand)
            :base (TC.COMBINATION_1)
        {
            if (rator == null)
                throw new ArgumentNullException ("rator");
            if (rand == null)
                throw new ArgumentNullException ("rand");
            this.rator = rator;
            this.rand = rand;
        }

        public Combination1 (object rator, object rand)
            : base (TC.COMBINATION_1)
        {
            this.rator = EnsureSCode (rator);
            this.rand = EnsureSCode (rand);
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

        public override SCode Bind (CompileTimeEnvironment ctenv)
        {
            SCode optimizedRator = this.rator.Bind (ctenv);
            SCode optimizedRand = this.rand.Bind (ctenv);
            return optimizedRator == this.rator && optimizedRand == this.rand
                ? this
                : new Combination1 (optimizedRator, optimizedRand);
        }

        public override bool EvalStep (out object answer, ref SCode expression, ref Environment environment)
        {
#if DEBUG
            Combination1.evaluationCount += 1;
#endif

            object evarg = null;
            SCode unev = this.rand;
            Environment env = environment;
            while (unev.EvalStep (out evarg, ref unev, ref env)) { };
            if (evarg == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination1Frame0 (this, environment));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }

            object evop = null;
            SCode unevop = this.rator;
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

        public override HashSet<string> FreeVariables ()
        {
            HashSet<string> freeVariables = this.rator.FreeVariables ();
            freeVariables.UnionWith (this.rand.FreeVariables ());
            return freeVariables;
        }

        public override bool NeedsTheEnvironment ()
        {
            return this.rator.NeedsTheEnvironment ()
                || this.rand.NeedsTheEnvironment ();
        }
    }

    sealed class Combination1Frame0 : SubproblemContinuation<Combination1>, ISystemVector
    {

        public Combination1Frame0 (Combination1 combination1, Environment environment)
            :base (combination1, environment)
        {
        }

        //public override bool EvalStep (out object answer, ref SCode expression, ref Environment environment)
        //{
        //    object evarg;
        //    SCode expr = ((RewindState) environment).PopFrame ();
        //    Environment env = environment;
        //    while (expr.EvalStep (out evarg, ref expr, ref env)) { };
        //    if (evarg == Interpreter.UnwindStack) {
        //        ((UnwinderState) env).AppendContinuationFrames (this.continuation);
        //        //((UnwinderState) env).AppendContinuationFrames ((RewindState) environment.OldFrames);
        //        environment = env;
        //        answer = Interpreter.UnwindStack;
        //        return false;
        //    }

        //    object evop = null;
        //    SCode unevop = this.combination1.Operator;
        //    env = this.environment;
        //    while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
        //    if (evop == Interpreter.UnwindStack) throw new NotImplementedException ();

        //    return Interpreter.Call (out answer, ref expression, ref environment, evop, evarg);

        //}

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

        public override bool Continue (out object answer, ref SCode expression, ref Environment environment, object value)
        {
            object evop = null;
            SCode unevop = this.expression.Operator;
            Environment env = this.environment;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
            if (evop == Interpreter.UnwindStack) throw new NotImplementedException ();

            return Interpreter.Call (out answer, ref expression, ref environment, evop, value);
        }
    }

    sealed class Combination1Frame1 : SubproblemContinuation<Combination1>, ISystemVector
    {
        object evarg;

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

        public override bool Continue (out object answer, ref SCode expression, ref Environment environment, object value)
        {
            throw new NotImplementedException ();
        }
    }


    //sealed class Combination1First : Subproblem<Combination1>
    //{
    //    [DebuggerStepThrough]
    //    public Combination1First (Continuation next, Combination1 expression, Environment environment)
    //        : base (next, expression, environment)
    //    {
    //    }

    //    internal override object Invoke (Interpreter interpreter, object value)
    //    {
    //        return interpreter.EvalReuseSubproblem (this.Expression.Operator, this.Environment, new Combination1Apply (this.Parent, value));
    //    }

    //    public override int FrameSize
    //    {
    //        get { return 3; }
    //    }

    //    public override object FrameRef (int offset)
    //    {
    //        switch (offset)
    //        {
    //            case 0:
    //                return ReturnCode.COMB_1_PROCEDURE;
    //            case 1:
    //                return this.Expression;
    //            case 2:
    //                return this.Environment;
    //            default:
    //                throw new NotImplementedException ();
    //        }
    //    }
    //}

    //sealed class Combination1Apply : Continuation
    //{
    //    [DebuggerBrowsable (DebuggerBrowsableState.Never)]
    //    readonly object rand;

    //    public Combination1Apply (Continuation next, object rand)
    //        : base (next)
    //    {
    //        this.rand = rand;
    //    }
 
    //    internal override object Invoke (Interpreter interpreter, object value)
    //    {
    //        return interpreter.CallProcedure ((SCode) value, this.rand);
    //    }

    //    public override int FrameSize
    //    {
    //        get { return 2; }
    //    }
    //}


    class Combination2 : SCode, ISystemHunk3
    {
#if DEBUG
        static long evaluationCount;
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
            this.rator = EnsureSCode (rator);
            this.rand0 = EnsureSCode (rand0);
            this.rand1 = EnsureSCode (rand1);
        }

        public Combination2 (Hunk3 init)
            : base (TC.COMBINATION_2)
        {
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

        public override SCode Bind (CompileTimeEnvironment ctenv)
        {
            return new Combination2 (this.rator.Bind (ctenv),
                                     this.rand0.Bind (ctenv),
                                     this.rand1.Bind (ctenv));
        }

        public override bool EvalStep (out object answer, ref SCode expression, ref Environment environment)
        {
#if DEBUG
            Combination2.evaluationCount += 1;
#endif

            object ev1 = null;
            Environment env = environment;
            SCode unev1 = this.rand1;
            while (unev1.EvalStep (out ev1, ref unev1, ref env)) { };
            if (ev1 == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new Combination2Frame0 (this, environment));
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

            object ev0 = null;
            env = environment;
            SCode unev0 = this.rand0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
            }

            object evop = null;
            env = environment;
            SCode unevop = this.rator;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            // expression = (SCode) evop;
            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, ev1);
        }

        public override HashSet<string> FreeVariables ()
        {
            HashSet<string> freeVariables = this.rator.FreeVariables ();
            freeVariables.UnionWith (this.rand0.FreeVariables ());
            freeVariables.UnionWith (this.rand1.FreeVariables ());
            return freeVariables;
        }

        public override bool NeedsTheEnvironment ()
        {
            return this.rator.NeedsTheEnvironment ()
            || this.rand0.NeedsTheEnvironment ()
            || this.rand1.NeedsTheEnvironment ();
        }
    }
    class Combination2Frame0 : SubproblemContinuation<Combination2>, ISystemVector
    {

        public Combination2Frame0 (Combination2 combination2, Environment environment)
            : base (combination2, environment)
        {
        }

        //public override bool EvalStep (out object answer, ref SCode expression, ref Environment environment)
        //{
        //    object evarg;
        //    SCode expr = ((RewindState) environment).PopFrame ();
        //    Environment env = environment;
        //    while (expr.EvalStep (out evarg, ref expr, ref env)) { };
        //    if (evarg == Interpreter.UnwindStack) {
        //        ((UnwinderState) env).AppendContinuationFrames (this.continuation);
        //        //((UnwinderState) env).AppendContinuationFrames ((RewindState) environment.OldFrames);
        //        environment = env;
        //        answer = Interpreter.UnwindStack;
        //        return false;
        //    }

        //    throw new NotImplementedException ();

        //}

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

        public override bool Continue (out object answer, ref SCode expression, ref Environment environment, object value)
        {
            object ev0 = null;
            Environment env = environment;
            SCode unev0 = this.expression.Rand0;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
            if (ev0 == Interpreter.UnwindStack) {
            }

            object evop = null;
            env = environment;
            SCode unevop = this.expression.Rator;
            while (unevop.EvalStep (out evop, ref unevop, ref env)) { };
            if (evop == Interpreter.UnwindStack) {
                throw new NotImplementedException ();
            }

            return Interpreter.Call (out answer, ref expression, ref environment, evop, ev0, value);
        }
    }
    //sealed class Combination2First : Subproblem<Combination2>
    //{
    //    public Combination2First (Continuation next, Combination2 expression, Environment environment)
    //        : base (next, expression, environment)
    //    {
    //    }

    //    internal override object Invoke (Interpreter interpreter, object value)
    //    {
    //        return interpreter.EvalReuseSubproblem (this.Expression.Rand1, this.Environment, new Combination2Second (this.Parent, this.Expression, value, this.Environment));
    //    }

    //    public override int FrameSize
    //    {
    //        get { throw new NotImplementedException (); }
    //    }
    //}

    //sealed class Combination2Second : Subproblem<Combination2>
    //{
    //    [DebuggerBrowsable (DebuggerBrowsableState.Never)]
    //    readonly object rand0;

 
    //    public Combination2Second (Continuation next, Combination2 expression, object rand0, Environment environment)
    //        : base (next, expression, environment)
    //    {
    //        this.rand0 = rand0;
    //    }

    //    internal override object Invoke (Interpreter interpreter, object value)
    //    {
    //        return interpreter.EvalReuseSubproblem (this.Expression.Rator, this.Environment, new Combination2Apply (this.Parent, this.rand0, value));
    //    }

    //    public object Evaluated
    //    {
    //        get
    //        {
    //            return new object [] { rand0 };
    //        }
    //    }

    //    public override int FrameSize
    //    {
    //        get { throw new NotImplementedException (); }
    //    }
    //}

    //sealed class Combination2Apply : Continuation
    //{
    //    [DebuggerBrowsable (DebuggerBrowsableState.Never)]
    //    readonly object rand0;

    //    [DebuggerBrowsable (DebuggerBrowsableState.Never)]
    //    readonly object rand1;

    //    public Combination2Apply (Continuation next, object rand0, object rand1)
    //        : base (next)
    //    {
    //        this.rand0 = rand0;
    //        this.rand1 = rand1;
    //    }

    //    internal override object Invoke (Interpreter interpreter, object value)
    //    {
    //        return interpreter.CallProcedure ((SCode) value, this.rand0, this.rand1);
    //    }

    //    public object Evaluated
    //    {
    //        get
    //        {
    //            return new object [] { rand0, rand1 };
    //        }
    //    }

    //    public override int FrameSize
    //    {
    //        get { throw new NotImplementedException (); }
    //    }
    //}

    sealed class PrimitiveCombination0 : SCode
    {
#if DEBUG
        static long evaluationCount;
#endif

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly Primitive0 procedure;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly PrimitiveMethod0 method;

        public PrimitiveCombination0 (Primitive0 procedure)
            : base (TC.PCOMB0)
        {
            if (procedure == null) throw new ArgumentNullException ("procedure");
            this.procedure = procedure;
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

        public override SCode Bind (CompileTimeEnvironment ctenv)
        {
            return this;
        }

        public override bool EvalStep (out object answer, ref SCode expression,ref Environment environment)
        {
#if DEBUG
            PrimitiveCombination0.evaluationCount += 1;
#endif 
            if (this.method (out answer)) {
                throw new NotImplementedException ();
            }
            else return false;
        }

        public override HashSet<string> FreeVariables ()
        {
            return new HashSet<string>();
        }

        public override bool NeedsTheEnvironment ()
        {
            return false;
        }
    }

    sealed class PrimitiveCombination1 : SCode
    {
#if DEBUG
        static long evaluationCount;
#endif
 
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        Primitive1 procedure;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        PrimitiveMethod1 method;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        SCode arg0;

        PrimitiveCombination1 (Primitive1 procedure,  SCode arg0)
            : base (TC.PCOMB1)
        {
            this.procedure = procedure;
            this.method = procedure.Method;
            this.arg0 = arg0;
        }

        public static SCode Make (Primitive1 rator, object rand)
        {
            if (rator == null) throw new ArgumentNullException ("rator");
            return new PrimitiveCombination1 (rator, EnsureSCode(rand));
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

        public override SCode Bind (CompileTimeEnvironment ctenv)
        {
            SCode optArg0 = this.arg0.Bind (ctenv);
            return optArg0 == this.arg0
                ? this
                : new PrimitiveCombination1 (this.procedure, optArg0);
        }

        public override bool EvalStep (out object answer, ref SCode expression,ref Environment environment)
        {
#if DEBUG
            PrimitiveCombination1.evaluationCount += 1;
#endif
            // Copying these are useless here.  Just bash them.
            SCode unev0 = this.arg0;
            // expression = this.arg0;
            Environment env = environment;
            object ev0 = null;
            while (unev0.EvalStep (out ev0, ref unev0, ref env)) { };
	        if (ev0 == Interpreter.UnwindStack) throw new NotImplementedException();

            // It is expensive to bounce down to invoke the procedure
            // we invoke it directly and pass along the ref args.

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

        public override HashSet<string> FreeVariables ()
        {
            return this.arg0.FreeVariables();
        }

        public override bool NeedsTheEnvironment ()
        {
            return this.arg0.NeedsTheEnvironment();
        }
    }

    //sealed class PrimitiveCombination1Apply : Continuation
    //{
    //    [DebuggerBrowsable (DebuggerBrowsableState.Never)]
    //    readonly Primitive1 rator;

    //    public PrimitiveCombination1Apply (Continuation next, Primitive1 rator)
    //        : base (next)
    //    {
    //        this.rator = rator;
    //    }

    //    internal override object Invoke (Interpreter interpreter, object value)
    //    {
    //        return interpreter.CallPrimitive (this.rator, value);
    //    }

    //    public Primitive1 Operator
    //    {
    //        [DebuggerStepThrough]
    //        get
    //        {
    //            return this.rator;
    //        }
    //    }

    //    public override int FrameSize
    //    {
    //        get { throw new NotImplementedException (); }
    //    }
    //}


    sealed class PrimitiveCombination2 : SCode, ISystemHunk3
    {
#if DEBUG
        static long evaluationCount;
#endif

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly Primitive2 rator;
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly PrimitiveMethod2 method;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode rand0;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode rand1;

        public PrimitiveCombination2 (Primitive2 rator, object rand0, object rand1)
            : base (TC.PCOMB2)
        {
            this.rator = rator;
            this.method = rator.Method;
            this.rand0 = EnsureSCode(rand0);
            this.rand1 = EnsureSCode(rand1);
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
            return new PrimitiveCombination2 (rator, rand0, rand1);
        }

        public static SCode Make (Hunk3 init)
        {
            return Make ((Primitive2) init.Cxr0, init.Cxr1, init.Cxr2);
        }

         public override SCode Bind (CompileTimeEnvironment ctenv)
        {
            SCode optRand0 = this.rand0.Bind (ctenv);
            SCode optRand1 = this.rand1.Bind (ctenv);
            return optRand0 == this.rand0
                && optRand1 == this.rand1
                ? this
                : new PrimitiveCombination2 (this.rator, optRand0, optRand1);
        }

        public override bool EvalStep (out object answer, ref SCode expression, ref Environment environment)
        {
#if DEBUG
            PrimitiveCombination2.evaluationCount += 1;
#endif
            SCode unev = this.rand1;
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

        public override HashSet<string> FreeVariables ()
        {
            HashSet<string> freeVariables = this.rand0.FreeVariables ();
            freeVariables.UnionWith (this.rand1.FreeVariables ());
            return freeVariables;
        }

        public override bool NeedsTheEnvironment ()
        {
            return this.rand0.NeedsTheEnvironment ()
                || this.rand1.NeedsTheEnvironment ();
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
    }

    sealed class PrimitiveCombination2Frame0 : SubproblemContinuation<PrimitiveCombination2>, ISystemVector
    {

        public PrimitiveCombination2Frame0 (PrimitiveCombination2 expression, Environment environment)
            : base (expression, environment)
        {
        }

        //public override bool EvalStep (out object answer, ref SCode expression, ref Environment environment)
        //{
        //    throw new NotImplementedException ();
        //}

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

        public override bool Continue (out object answer, ref SCode expression, ref Environment environment, object value)
        {
            throw new NotImplementedException ();
        }
    }

    sealed class PrimitiveCombination2Frame1 : SubproblemContinuation<PrimitiveCombination2>, ISystemVector
    {
        object ev1;

                internal PrimitiveCombination2Frame1 (PrimitiveCombination2 expression, Environment environment, object ev1)
                    :base (expression, environment)
        {
            this.ev1 = ev1;
        }
        //public override bool EvalStep (out object answer, ref SCode expression, ref Environment environment)
        //{
        //    throw new NotImplementedException ();
        //}

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

        public override bool Continue (out object answer, ref SCode expression, ref Environment environment, object value)
        {
            throw new NotImplementedException ();
        }
    }


    //sealed class PrimitiveCombination2First : Subproblem<PrimitiveCombination2>
    //{
    //    public PrimitiveCombination2First (Continuation next, PrimitiveCombination2 expression, Environment environment)
    //        : base (next, expression, environment)
    //    {
    //    }

    //    internal override object Invoke (Interpreter interpreter, object value)
    //    {
    //        return interpreter.EvalReuseSubproblem (this.Expression.Rand1, this.Environment, new PrimitiveCombination2Apply (this.Parent, this.Expression.Rator, value));
    //    }

    //    public override int FrameSize
    //    {
    //        get { throw new NotImplementedException (); }
    //    }
    //}

    //sealed class PrimitiveCombination2Apply : Continuation
    //{
    //    [DebuggerBrowsable (DebuggerBrowsableState.Never)]
    //    readonly Primitive2 rator;

    //    [DebuggerBrowsable (DebuggerBrowsableState.Never)]
    //    readonly object rand0;

    //    public PrimitiveCombination2Apply (Continuation next, Primitive2 rator, object rand0)
    //        : base (next)
    //    {
    //        this.rator = rator;
    //        this.rand0 = rand0;
    //    }

    //    internal override object Invoke (Interpreter interpreter, object value)
    //    {
    //        return interpreter.CallPrimitive (this.rator, this.rand0, value);
    //    }

    //    public override int FrameSize
    //    {
    //        get { throw new NotImplementedException (); }
    //    }
    //}

    sealed class PrimitiveCombination3 : SCode
    {
#if DEBUG
        static long evaluationCount;
#endif
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        Primitive3 procedure;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        PrimitiveMethod3 method;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        SCode arg0;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        SCode arg1;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        SCode arg2;

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

        //[SchemePrimitive ("PRIMITIVE-COMBINATION3?", 1)]
        //public static PartialResult IsPrimitiveCombination3 (Interpreter interpreter, object arg)
        //{
        //    return new PartialResult (arg is PrimitiveCombination3);
        //}

        public override SCode Bind (CompileTimeEnvironment ctenv)
        {
            return new PrimitiveCombination3 (this.procedure,
                this.Operand0.Bind (ctenv),
                this.Operand1.Bind (ctenv),
                this.Operand2.Bind (ctenv));
        }

        public override bool EvalStep (out object answer, ref SCode expression, ref Environment environment)
        {
#if DEBUG
            PrimitiveCombination3.evaluationCount += 1;
#endif
            object ev2 = null;
            Environment env = environment;
            SCode unev = this.arg2;
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

        public override HashSet<string> FreeVariables ()
        {
            HashSet<string> freeVariables = this.arg0.FreeVariables ();
            freeVariables.UnionWith (this.arg1.FreeVariables ());
            freeVariables.UnionWith (this.arg2.FreeVariables ());
            return freeVariables;
        }

        public override bool NeedsTheEnvironment ()
        {
            return this.arg0.NeedsTheEnvironment ()
                || this.arg1.NeedsTheEnvironment ()
                || this.arg2.NeedsTheEnvironment ();
        }

    }

    //sealed class PrimitiveCombination3First : Subproblem<PrimitiveCombination3>
    //{
    //    public PrimitiveCombination3First (Continuation next, PrimitiveCombination3 expression, Environment environment)
    //        : base (next, expression, environment)
    //    {
    //    }

 
    //    internal override object Invoke (Interpreter interpreter, object value)
    //    {
    //         return interpreter.EvalReuseSubproblem (this.Expression.Operand1, this.Environment, new PrimitiveCombination3Second (this.Parent, this.Expression, this.Environment, value));
    //    }

    //    public override int FrameSize
    //    {
    //        get { throw new NotImplementedException (); }
    //    }
    //}

    //sealed class PrimitiveCombination3Second : Subproblem<PrimitiveCombination3>
    //{
    //    readonly object rand0;

    //    public PrimitiveCombination3Second (Continuation next, PrimitiveCombination3 expression, Environment environment, object rand0)
    //        : base (next, expression, environment)
    //    {
    //        this.rand0 = rand0;
    //    }

    //    internal override object Invoke (Interpreter interpreter, object value)
    //    {
    //        return interpreter.EvalReuseSubproblem (this.Expression.Operand2, this.Environment, new PrimitiveCombination3Apply (this.Parent, this.Expression.Operator, this.rand0, value));
    //    }

    //    public override int FrameSize
    //    {
    //        get { throw new NotImplementedException (); }
    //    }
    //}

    //sealed class PrimitiveCombination3Apply : Continuation
    //{
    //    [DebuggerBrowsable (DebuggerBrowsableState.Never)]
    //    readonly Primitive3 rator;

    //    [DebuggerBrowsable (DebuggerBrowsableState.Never)]
    //    readonly object rand0;

    //    [DebuggerBrowsable (DebuggerBrowsableState.Never)]
    //    readonly object rand1;

    //    public PrimitiveCombination3Apply (Continuation next, Primitive3 rator, object rand0, object rand1)
    //        : base (next)
    //    {
    //        this.rator = rator;
    //        this.rand0 = rand0;
    //        this.rand1 = rand1;
    //    }


    //    internal override object Invoke (Interpreter interpreter, object value)
    //    {
    //        return interpreter.CallPrimitive (this.rator, this.rand0, this.rand1, value);
    //    }

    //    public override int FrameSize
    //    {
    //        get { throw new NotImplementedException (); }
    //    }
    //}

}
