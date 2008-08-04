using System;
using System.Collections.Generic;
using System.Diagnostics;

namespace Microcode
{
    sealed class Combination : SCode, ISystemVector
    {
        static long evaluationCount;
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode [] components;

        public Combination (SCode [] components)
        {
            this.components = components;
        }

        public Combination (object [] components)
        {
            SCode[] comps = new SCode [components.Length];
            for (int i = 0; i < components.Length; i++)
                comps [i] = EnsureSCode (components [i]);
            this.components = comps;
        }

        public Combination(Cons components)
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

        internal override object EvalStep (Interpreter interpreter, object etc)
        {
            Combination.evaluationCount += 1;
            if (components.Length == 1)
            {
                return interpreter.EvalNewSubproblem (components [0], new CombinationApply (interpreter.Continuation, new object [] { }));
            }
            else
                return interpreter.EvalNewSubproblem (components [components.Length - 1],
                                                   new CombinationAccumulate (interpreter.Continuation, components,
                                                                              null,
                                                                              components.Length - 1,
                                                                              interpreter.Environment));
        }

        #region ISystemVector Members
        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        public int SystemVectorSize
        {
            get { return this.components.Length; }
        }

        public object SystemVectorRef (int index)
        {
            return UnwrapQuoted(this.components [index]);
        }

        public object SystemVectorSet (int index, object newValue)
        {
            throw new NotImplementedException ();
        }

        #endregion
    }

    sealed class CombinationAccumulate : Continuation
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode [] components;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly Cons values;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        int index;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly Environment environment;

        public CombinationAccumulate (Continuation next, SCode [] components, Cons values, int index, Environment environment)
            : base (next)
        {
            this.components = components;
            this.values = values;
            this.index = index;
            this.environment = environment;
        }

        internal override object Invoke (Interpreter interpreter, object value)
        {
            if (this.index == 1)
            {
                object [] valuevector = new object [this.components.Length - 1];
                Cons tail = this.values;
                int scan = 1;
                valuevector [0] = value;
                while (tail != null)
                {
                    valuevector [scan++] = tail.Car;
                    tail = (Cons) tail.Cdr;
                }
                return interpreter.EvalReuseSubproblem (this.components [0], this.environment, new CombinationApply (this.Parent, valuevector));
            }
            else
                return interpreter.EvalReuseSubproblem (this.components [this.index - 1], this.environment,
                                                   new CombinationAccumulate (this.Parent,
                                                                              this.components,
                                                                              new Cons (value, this.values),
                                                                              this.index - 1,
                                                                              this.environment));
        }

        public override int FrameSize
        {
            get { return 3 + this.components.Length; }
        }
    }

    sealed class CombinationApply : Continuation
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly object [] arguments;

        public CombinationApply (Continuation next, object [] arguments)
            : base (next)
        {
            this.arguments = arguments;
        }

        internal override object Invoke (Interpreter interpreter, object value)
        {
            return interpreter.Apply (value, arguments);
        }

        public override int FrameSize
        {
            get { throw new NotImplementedException (); }
        }
    }


    sealed class Combination1 : SCode, ISystemPair
    {
        static long evaluationCount;
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode rator;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode rand;

        public Combination1 (SCode rator, SCode rand)
        {
            if (rator == null)
                throw new ArgumentNullException ("rator");
            if (rand == null)
                throw new ArgumentNullException ("rand");
            this.rator = rator;
            this.rand = rand;
        }

        public Combination1 (object rator, object rand)
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

        internal override object EvalStep (Interpreter interpreter, object etc)
        {
            Combination1.evaluationCount += 1;
            return interpreter.EvalNewSubproblem (this.rand, new Combination1First (interpreter.Continuation, this, interpreter.Environment));
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
    }

    sealed class Combination1First : Subproblem<Combination1>
    {
        [DebuggerStepThrough]
        public Combination1First (Continuation next, Combination1 expression, Environment environment)
            : base (next, expression, environment)
        {
        }

        internal override object Invoke (Interpreter interpreter, object value)
        {
            return interpreter.EvalReuseSubproblem (this.Expression.Operator, this.Environment, new Combination1Apply (this.Parent, value));
        }

        public override int FrameSize
        {
            get { return 3; }
        }

        public override object FrameRef (int offset)
        {
            switch (offset)
            {
                case 0:
                    return ReturnCode.COMB_1_PROCEDURE;
                case 1:
                    return this.Expression;
                case 2:
                    return this.Environment;
                default:
                    throw new NotImplementedException ();
            }
        }
    }

    sealed class Combination1Apply : Continuation
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly object rand;

        public Combination1Apply (Continuation next, object rand)
            : base (next)
        {
            this.rand = rand;
        }
 
        internal override object Invoke (Interpreter interpreter, object value)
        {
            return interpreter.CallProcedure ((SCode) value, this.rand);
        }

        public override int FrameSize
        {
            get { return 2; }
        }
    }


    class Combination2 : SCode, ISystemHunk3
    {
        static long evaluationCount;
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode rator;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode rand0;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode rand1;

        public Combination2 (object rator, object rand0, object rand1)
        {
            this.rator = EnsureSCode(rator);
            this.rand0 = EnsureSCode (rand0);
            this.rand1 = EnsureSCode (rand1);
        }

        public Combination2 (Hunk3 init)
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

        internal override object EvalStep (Interpreter interpreter, object etc)
        {
            Combination2.evaluationCount += 1;
            return interpreter.EvalNewSubproblem (this.rand0, new Combination2First (interpreter.Continuation, this, interpreter.Environment));
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
                return UnwrapQuoted(rand0);
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
                return UnwrapQuoted(rand1);
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        #endregion
    }

    sealed class Combination2First : Subproblem<Combination2>
    {
        public Combination2First (Continuation next, Combination2 expression, Environment environment)
            : base (next, expression, environment)
        {
        }

        internal override object Invoke (Interpreter interpreter, object value)
        {
            return interpreter.EvalReuseSubproblem (this.Expression.Rand1, this.Environment, new Combination2Second (this.Parent, this.Expression, value, this.Environment));
        }

        public override int FrameSize
        {
            get { throw new NotImplementedException (); }
        }
    }

    sealed class Combination2Second : Subproblem<Combination2>
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly object rand0;

 
        public Combination2Second (Continuation next, Combination2 expression, object rand0, Environment environment)
            : base (next, expression, environment)
        {
            this.rand0 = rand0;
        }

        internal override object Invoke (Interpreter interpreter, object value)
        {
            return interpreter.EvalReuseSubproblem (this.Expression.Rator, this.Environment, new Combination2Apply (this.Parent, this.rand0, value));
        }

        public object Evaluated
        {
            get
            {
                return new object [] { rand0 };
            }
        }

        public override int FrameSize
        {
            get { throw new NotImplementedException (); }
        }
    }

    sealed class Combination2Apply : Continuation
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly object rand0;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly object rand1;

        public Combination2Apply (Continuation next, object rand0, object rand1)
            : base (next)
        {
            this.rand0 = rand0;
            this.rand1 = rand1;
        }

        internal override object Invoke (Interpreter interpreter, object value)
        {
            return interpreter.CallProcedure ((SCode) value, this.rand0, this.rand1);
        }

        public object Evaluated
        {
            get
            {
                return new object [] { rand0, rand1 };
            }
        }

        public override int FrameSize
        {
            get { throw new NotImplementedException (); }
        }
    }

    sealed class PrimitiveCombination0 : SCode
    {
        static long evaluationCount;
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly Primitive0 procedure;

        public PrimitiveCombination0 (Primitive0 procedure)
        {
            if (procedure == null) throw new ArgumentNullException ("procedure");
            this.procedure = procedure;
        }

        internal override object EvalStep (Interpreter interpreter, object etc)
        {
            PrimitiveCombination0.evaluationCount += 1;
            return interpreter.CallPrimitive (this.procedure);
        }
    }

    sealed class PrimitiveCombination1 : SCode
    {
        static long evaluationCount;
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        Primitive1 procedure;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        SCode arg0;

        PrimitiveCombination1 (Primitive1 procedure,  SCode arg0)
        {
            this.procedure = procedure;
            this.arg0 = arg0;
        }

        public static SCode Make (Primitive1 rator, object rand)
        {
            if (rator == null) throw new ArgumentNullException ("rator");
            if (rand == null) throw new ArgumentNullException ("rand");
            return new PrimitiveCombination1 (rator, EnsureSCode(rand));
        }

        internal override object EvalStep (Interpreter interpreter, object etc)
        {
            PrimitiveCombination1.evaluationCount += 1;
            return interpreter.EvalNewSubproblem (this.arg0, new PrimitiveCombination1Apply (interpreter.Continuation, this.procedure));
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
    }

    sealed class PrimitiveCombination1Apply : Continuation
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly Primitive1 rator;

        public PrimitiveCombination1Apply (Continuation next, Primitive1 rator)
            : base (next)
        {
            this.rator = rator;
        }

        internal override object Invoke (Interpreter interpreter, object value)
        {
            return interpreter.CallPrimitive (this.rator, value);
        }

        public Primitive1 Operator
        {
            [DebuggerStepThrough]
            get
            {
                return this.rator;
            }
        }

        public override int FrameSize
        {
            get { throw new NotImplementedException (); }
        }
    }


    class PrimitiveCombination2 : SCode, ISystemHunk3
    {
        static long evaluationCount;
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly Primitive2 rator;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode rand0;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode rand1;

        protected PrimitiveCombination2 (Primitive2 rator, object rand0, object rand1)
        {
            this.rator = rator;
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

        internal override object EvalStep (Interpreter interpreter, object etc)
        {
            PrimitiveCombination2.evaluationCount += 1;
            return interpreter.EvalNewSubproblem (this.rand0, new PrimitiveCombination2First (interpreter.Continuation, this, interpreter.Environment));

        }

        #region ISystemHunk3 Members
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemHunk3Cxr0
        {
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
        public object SystemHunk3Cxr1
        {
            get
            {
                return UnwrapQuoted(this.rand0);
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
                return UnwrapQuoted(this.rand1);
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        #endregion
    }

    sealed class PrimitiveCombination2First : Subproblem<PrimitiveCombination2>
    {
        public PrimitiveCombination2First (Continuation next, PrimitiveCombination2 expression, Environment environment)
            : base (next, expression, environment)
        {
        }

        internal override object Invoke (Interpreter interpreter, object value)
        {
            return interpreter.EvalReuseSubproblem (this.Expression.Rand1, this.Environment, new PrimitiveCombination2Apply (this.Parent, this.Expression.Rator, value));
        }

        public override int FrameSize
        {
            get { throw new NotImplementedException (); }
        }
    }

    sealed class PrimitiveCombination2Apply : Continuation
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly Primitive2 rator;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly object rand0;

        public PrimitiveCombination2Apply (Continuation next, Primitive2 rator, object rand0)
            : base (next)
        {
            this.rator = rator;
            this.rand0 = rand0;
        }

        internal override object Invoke (Interpreter interpreter, object value)
        {
            return interpreter.CallPrimitive (this.rator, this.rand0, value);
        }

        public override int FrameSize
        {
            get { throw new NotImplementedException (); }
        }
    }

    sealed class PrimitiveCombination3 : SCode
    {
        static long evaluationCount;
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        Primitive3 procedure;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        SCode arg0;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        SCode arg1;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        SCode arg2;

        public PrimitiveCombination3 (Primitive3 procedure, object arg0, object arg1, object arg2)
        {
            if (procedure == null) throw new ArgumentNullException ("procedure");
            this.procedure = procedure;
            this.arg0 = EnsureSCode(arg0);
            this.arg1 = EnsureSCode(arg1);
            this.arg2 = EnsureSCode(arg2);
        }

        internal override object EvalStep (Interpreter interpreter, object etc)
        {
            PrimitiveCombination3.evaluationCount += 1;
            return interpreter.EvalNewSubproblem (this.arg0, new PrimitiveCombination3First (interpreter.Continuation, this, interpreter.Environment));
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

        [SchemePrimitive ("PRIMITIVE-COMBINATION3?", 1)]
        public static void IsPrimitiveCombination3 (Interpreter interpreter, object arg)
        {
            interpreter.Return (arg is PrimitiveCombination3);
        }
    }

    sealed class PrimitiveCombination3First : Subproblem<PrimitiveCombination3>
    {
        public PrimitiveCombination3First (Continuation next, PrimitiveCombination3 expression, Environment environment)
            : base (next, expression, environment)
        {
        }

 
        internal override object Invoke (Interpreter interpreter, object value)
        {
             return interpreter.EvalReuseSubproblem (this.Expression.Operand1, this.Environment, new PrimitiveCombination3Second (this.Parent, this.Expression, this.Environment, value));
        }

        public override int FrameSize
        {
            get { throw new NotImplementedException (); }
        }
    }

    sealed class PrimitiveCombination3Second : Subproblem<PrimitiveCombination3>
    {
        readonly object rand0;

        public PrimitiveCombination3Second (Continuation next, PrimitiveCombination3 expression, Environment environment, object rand0)
            : base (next, expression, environment)
        {
            this.rand0 = rand0;
        }

        internal override object Invoke (Interpreter interpreter, object value)
        {
            return interpreter.EvalReuseSubproblem (this.Expression.Operand2, this.Environment, new PrimitiveCombination3Apply (this.Parent, this.Expression.Operator, this.rand0, value));
        }

        public override int FrameSize
        {
            get { throw new NotImplementedException (); }
        }
    }

    sealed class PrimitiveCombination3Apply : Continuation
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly Primitive3 rator;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly object rand0;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly object rand1;

        public PrimitiveCombination3Apply (Continuation next, Primitive3 rator, object rand0, object rand1)
            : base (next)
        {
            this.rator = rator;
            this.rand0 = rand0;
            this.rand1 = rand1;
        }


        internal override object Invoke (Interpreter interpreter, object value)
        {
            return interpreter.CallPrimitive (this.rator, this.rand0, this.rand1, value);
        }

        public override int FrameSize
        {
            get { throw new NotImplementedException (); }
        }
    }

}
