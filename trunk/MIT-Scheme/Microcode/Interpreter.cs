using System;
using System.Collections.Generic;
using System.Diagnostics;

namespace Microcode
{
    public enum ReturnCode
    {
        END_OF_COMPUTATION = 0x00,
        /* formerly RC_RESTORE_CONTROL_POINT	= 0x01 */
        JOIN_STACKLETS = 0x01,
        RESTORE_CONTINUATION = 0x02, /* Used for 68000 */
        INTERNAL_APPLY = 0x03,
        BAD_INTERRUPT_CONTINUE = 0x04, /* Used for 68000 */
        RESTORE_HISTORY = 0x05,
        INVOKE_STACK_THREAD = 0x06,
        RESTART_EXECUTION = 0x07, /* Used for 68000 */
        EXECUTE_ASSIGNMENT_FINISH = 0x08,
        EXECUTE_DEFINITION_FINISH = 0x09,
        EXECUTE_ACCESS_FINISH = 0x0A,
        EXECUTE_IN_PACKAGE_CONTINUE = 0x0B,
        SEQ_2_DO_2 = 0x0C,
        SEQ_3_DO_2 = 0x0D,
        SEQ_3_DO_3 = 0x0E,
        CONDITIONAL_DECIDE = 0x0F,
        DISJUNCTION_DECIDE = 0x10,
        COMB_1_PROCEDURE = 0x11,
        COMB_APPLY_FUNCTION = 0x12,
        COMB_2_FIRST_OPERAND = 0x13,
        COMB_2_PROCEDURE = 0x14,
        COMB_SAVE_VALUE = 0x15,
        PCOMB1_APPLY = 0x16,
        PCOMB2_DO_1 = 0x17,
        PCOMB2_APPLY = 0x18,
        PCOMB3_DO_2 = 0x19,
        PCOMB3_DO_1 = 0x1A,
        PCOMB3_APPLY = 0x1B,
        SNAP_NEED_THUNK = 0x1C,
        REENTER_COMPILED_CODE = 0x1D,
        /* formerly RC_GET_CHAR_REPEAT		= 0x1E */
        COMP_REFERENCE_RESTART = 0x1F,
        NORMAL_GC_DONE = 0x20,
        COMPLETE_GC_DONE = 0x21, /* Used for 68000 */
        PURIFY_GC_1 = 0x22,
        PURIFY_GC_2 = 0x23,
        AFTER_MEMORY_UPDATE = 0x24, /* Used for 68000 */
        RESTARTABLE_EXIT = 0x25, /* Used for 68000 */
        /* formerly RC_GET_CHAR 		= 0x26 */
        /* formerly RC_GET_CHAR_IMMEDIATE	= 0x27 */
        COMP_ASSIGNMENT_RESTART = 0x28,
        POP_FROM_COMPILED_CODE = 0x29,
        RETURN_TRAP_POINT = 0x2A,
        RESTORE_STEPPER = 0x2B, /* Used for 68000 */
        RESTORE_TO_STATE_POINT = 0x2C,
        MOVE_TO_ADJACENT_POINT = 0x2D,
        RESTORE_VALUE = 0x2E,
        RESTORE_DONT_COPY_HISTORY = 0x2F,

        /* The following are not used in the 68000 implementation */
        POP_RETURN_ERROR = 0x40,
        EVAL_ERROR = 0x41,
        STACK_MARKER = 0x42,
        COMP_INTERRUPT_RESTART = 0x43,
        /* formerly RC_COMP_RECURSION_GC	= 0x44 */
        RESTORE_INT_MASK = 0x45,
        HALT = 0x46,
        FINISH_GLOBAL_INT = 0x47,	/* Multiprocessor */
        REPEAT_DISPATCH = 0x48,
        GC_CHECK = 0x49,
        RESTORE_FLUIDS = 0x4A,
        COMP_LOOKUP_APPLY_RESTART = 0x4B,
        COMP_ACCESS_RESTART = 0x4C,
        COMP_UNASSIGNED_P_RESTART = 0x4D,
        COMP_UNBOUND_P_RESTART = 0x4E,
        COMP_DEFINITION_RESTART = 0x4F,
        /* formerly RC_COMP_LEXPR_INTERRUPT_RESTART = 0x50 */
        COMP_SAFE_REFERENCE_RESTART = 0x51,
        /* formerly RC_COMP_CACHE_LOOKUP_RESTART  	= 0x52 */
        COMP_LOOKUP_TRAP_RESTART = 0x53,
        COMP_ASSIGNMENT_TRAP_RESTART = 0x54,
        /* formerly RC_COMP_CACHE_OPERATOR_RESTART	= 0x55 */
        COMP_OP_REF_TRAP_RESTART = 0x56,
        COMP_CACHE_REF_APPLY_RESTART = 0x57,
        COMP_SAFE_REF_TRAP_RESTART = 0x58,
        COMP_UNASSIGNED_TRAP_RESTART = 0x59,
        /* formerly RC_COMP_CACHE_ASSIGN_RESTART	= 0x5A */
        COMP_LINK_CACHES_RESTART = 0x5B,
        HARDWARE_TRAP = 0x5C,
        INTERNAL_APPLY_VAL = 0x5D,
        COMP_ERROR_RESTART = 0x5E,
        PRIMITIVE_CONTINUE = 0x5F

        /* When adding return codes, add them to the table below as well! */

        // #define MAX_RETURN_CODE			0x5F
    }

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
        History history;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        int interrupt_mask;

        public Interpreter ()
        { }

        public Termination Start (SCode initialForm)
        {
            this.continuation = new ExitInterpreter ();
            this.expression = initialForm;
            this.environment = Environment.Global;
            this.history = FixedObjectsVector.TheDummyHistory;

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
            // than a statement so we chain the return value
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
            this.history.NewReduction (this.expression, this.environment);
            return null;
        }

        [DebuggerStepThrough]
        internal object EvalReduction (SCode sCode, Environment environment)
        {
            this.expression = sCode;
            this.environment = environment;
            this.history.NewReduction (this.expression, this.environment);
            return null;
        }

        [DebuggerStepThrough]
        internal object EvalNewSubproblem (SCode sCode, Continuation continuation)
        {
            this.expression = sCode;
            this.continuation = continuation;
            this.history.NewSubproblem (sCode, this.environment);
            return null;
        }

        [DebuggerStepThrough]
        internal object EvalNewSubproblem (SCode sCode, Environment environment, Continuation continuation)
        {
            this.expression = sCode;
            this.environment = environment;
            this.continuation = continuation;
            this.history.NewSubproblem (sCode, environment);
            return null;
        }

        [DebuggerStepThrough]
        internal object EvalReuseSubproblem (SCode sCode, Continuation continuation)
        {
            this.expression = sCode;
            this.continuation = continuation;
            this.history.ReuseSubproblem (sCode, this.environment);
            return null;
        }

        [DebuggerStepThrough]
        internal object EvalReuseSubproblem (SCode sCode, Environment environment, Continuation continuation)
        {
            this.expression = sCode;
            this.environment = environment;
            this.continuation = continuation;
            this.history.ReuseSubproblem (sCode, environment);
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

        internal SCode Expression
        {
            [DebuggerStepThrough]
            get
            {
                return this.expression;
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

        History History
        {
            [DebuggerStepThrough]
            get
            {
                return this.history;
            }
            set
            {
                this.history = value;
            }
        }

        [SchemePrimitive ("APPLY", 2)]
        public static object UserApply (Interpreter interpreter, object arg0, object arg1)
        {
            return interpreter.Apply (arg0, ((Cons) (arg1)).ToVector ());
        }

        [SchemePrimitive ("CALL-WITH-CURRENT-CONTINUATION", 1)]
        public static object CallWithCurrentContinuation (Interpreter interpreter, object arg)
        {
            return interpreter.Apply (arg,
                new object [] { new ControlPoint (interpreter.InterruptMask,
                                                   interpreter.History, 
                                                   interpreter.Continuation) });
        }

        [SchemePrimitive ("GET-INTERRUPT-ENABLES", 0)]
        public static object GetInterruptEnables (Interpreter interpreter)
        {
            return interpreter.Return (0);
        }

        [SchemePrimitive ("SCODE-EVAL", 2)]
        public static object ScodeEval (Interpreter interpreter, object arg0, object arg1)
        {

            SCode sarg0 = arg0 as SCode;
            Environment earg1 = arg1 as Environment;
            return interpreter.EvalReduction (sarg0 == null ? Quotation.Make (arg0) : sarg0, earg1);
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

        [SchemePrimitive ("WITH-STACK-MARKER", 3)]
        public static object WithStackMarker (Interpreter interpreter, object thunk, object mark1, object mark2)
        {
            interpreter.Continuation = new StackMarker (interpreter.Continuation, mark1, mark2);
            return interpreter.Apply (thunk, new object [] { });
        }
    }
}
