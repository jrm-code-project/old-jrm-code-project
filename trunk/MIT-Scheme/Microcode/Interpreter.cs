using System;
using System.Collections.Generic;
using System.Diagnostics;

namespace Microcode
{
    public sealed class ExitInterpreterException : Exception
    {
        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        readonly Termination termination;

        public ExitInterpreterException (Termination termination)
        {
            this.termination = termination;
        }

        public Termination Termination
        {
            [DebuggerStepThrough]
            get
            {
                return this.termination;
            }
        }
    }

    public sealed class Interpreter
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        SCode expression;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        Environment environment;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        Continuation continuation;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        object [] arguments;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        object primitiveArgument0;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        object primitiveArgument1;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        object primitiveArgument2;



        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        int interrupt_mask;

        public Interpreter ()
        { }

        public Termination Start (SCode initialForm)
        {
            this.expression = initialForm;
            this.environment = Environment.Global;
            try
            {
                Run ();
                return Termination.UNEXPECTED_EXIT;
            }
            catch (ExitInterpreterException exit)
            {
                return exit.Termination;
            }
        }

        void Run ()
        {
            // The etc argument isn't used.
            // We want EvalStep to be an expression rather
            // than a statement, so we chain the return value
            // around in case we think of a use in the
            // future.
            object etc = null;
            while (true)
                etc = this.expression.EvalStep (this, etc);
        }

        [DebuggerStepThrough]
        internal object EvalReduction (SCode sCode)
        {
            this.expression = sCode;
            return null;
        }

        [DebuggerStepThrough]
        internal object EvalReduction (SCode sCode, Environment environment)
        {
            this.expression = sCode;
            this.environment = environment;
            return null;
        }

        [DebuggerStepThrough]
        internal object EvalSubproblem (SCode sCode, Continuation continuation)
        {
            this.expression = sCode;
            this.continuation = continuation;
            return null;
        }

        [DebuggerStepThrough]
        internal object EvalSubproblem (SCode sCode, Environment environment, Continuation continuation)
        {
            this.expression = sCode;
            this.environment = environment;
            this.continuation = continuation;
            return null;
        }

        internal object Apply (object rator, object [] rands)
        {
            Primitive primrator = rator as Primitive;
            if (primrator == null)
            {
                this.expression = (SCode) rator;
                this.arguments = rands;
                return this.expression;
            }
            else switch (primrator.Arity)
                {
                    case 0:
                        return CallPrimitive ((Primitive0) rator);
                    case 1:
                        return CallPrimitive ((Primitive1) rator, rands [0]);
                    case 2:
                        return CallPrimitive ((Primitive2) rator, rands [0], rands [1]);
                    case 3:
                        return CallPrimitive ((Primitive3) rator, rands [0], rands [1], rands [3]);
                    default:
                        this.expression = (SCode) rator;
                        this.arguments = rands;
                        return this.expression;
                }
        }

        [DebuggerStepThrough]
        internal object CallPrimitive (Primitive0 rator)
        {
            this.expression = rator;
            return null;
        }

        [DebuggerStepThrough]
        internal object CallPrimitive (Primitive1 rator, object argument)
        {
            this.expression = rator;
            this.primitiveArgument0 = argument;
            return null;
        }

        [DebuggerStepThrough]
        internal object CallPrimitive (Primitive2 rator, object argument0, object argument1)
        {
            this.expression = rator;
            this.primitiveArgument0 = argument0;
            this.primitiveArgument1 = argument1;
            return null;
        }

        [DebuggerStepThrough]
        internal object CallPrimitive (Primitive3 rator, object argument0, object argument1, object argument2)
        {
            this.expression = rator;
            this.primitiveArgument0 = argument0;
            this.primitiveArgument1 = argument1;
            this.primitiveArgument2 = argument2;
            return null;
        }

        internal object CallProcedure (SCode rator, object rand)
        {
            Primitive primval = rator as Primitive;
            if ((primval == null)
                || (primval.Arity == -1))
            {
                this.expression = rator;
                this.arguments = new object [] { rand };
                return null;
            }
            else if (primval.Arity == 1)
                return CallPrimitive ((Primitive1) primval, rand);
            else
                throw new NotImplementedException ();
        }

        internal object CallProcedure (SCode rator, object rand0, object rand1)
        {
            Primitive primval = rator as Primitive;
            if ((primval == null)
                || (primval.Arity == -1))
            {
                this.expression = rator;
                this.arguments = new object [] { rand0, rand1 };
                return rator;
            }
            else if (primval.Arity == 2)
                return CallPrimitive ((Primitive2) primval, rand0, rand1);
            else
                throw new NotImplementedException ();
        }

        internal object CallProcedure (SCode rator, object rand0, object rand1, object rand2)
        {
            Primitive primval = rator as Primitive;
            if ((primval == null)
                || (primval.Arity == -1))
            {
                this.expression = rator;
                this.arguments = new object [] { rand0, rand1, rand2 };
                return rator;
            }
            else if (primval.Arity == 3)
                return CallPrimitive ((Primitive3) primval, rand0, rand1, rand2);
            else
                throw new NotImplementedException ();
        }

        internal object Return (object value)
        {
            Continuation cont = this.continuation;
            this.continuation = cont.Parent;
            return cont.Invoke (this, value);
        }

        internal object Return ()
        {
            return Return (Constant.Unspecific);
        }

        internal Continuation Continuation
        {
            [DebuggerStepThrough]
            get
            {
                return this.continuation;
            }
            set
            {
                this.continuation = value;
            }
        }

        internal Environment Environment
        {
            [DebuggerStepThrough]
            get
            {
                return this.environment;
            }
        }

        internal object [] Arguments
        {
            [DebuggerStepThrough]
            get
            {
                return this.arguments;
            }
        }

        internal object PrimitiveArgument0
        {
            [DebuggerStepThrough]
            get
            {
                return this.primitiveArgument0;
            }
        }

        internal object PrimitiveArgument1
        {
            [DebuggerStepThrough]
            get
            {
                return this.primitiveArgument1;
            }
        }

        internal object PrimitiveArgument2
        {
            [DebuggerStepThrough]
            get
            {
                return this.primitiveArgument2;
            }
        }

        public int InterruptMask
        {
            [DebuggerStepThrough]
            get
            {
                return this.interrupt_mask;
            }
            set
            {
                this.interrupt_mask = value;
            }
        }

        [SchemePrimitive ("SCODE-EVAL", 2)]
        public static object ScodeEval (Interpreter interpreter, object arg0, object arg1)
        {
            Environment env = ((Environment) (arg1));
            if (env == null)
                env = Environment.Global;

            SCode sarg0 = arg0 as SCode;
            return interpreter.EvalReduction (sarg0 == null ? Quotation.Make (arg0) : sarg0, env);
        }
 
        [SchemePrimitive ("SET-INTERRUPT-ENABLES!", 1)]
        public static object SetInterruptEnables (Interpreter interpreter, object arg)
        {
            return interpreter.Return (null);
        }

        [SchemePrimitive ("WITH-INTERRUPT-MASK", 2)]
        public static object WithInterruptMask (Interpreter interpreter, object arg0, object arg1)
        {
            int oldMask = interpreter.InterruptMask;
            interpreter.InterruptMask = (int) (arg0);
            interpreter.Continuation = new RestoreInterruptMask (interpreter.Continuation, oldMask);
            return interpreter.Apply (arg1, new object [] { interpreter.InterruptMask });
        }
    }
}
