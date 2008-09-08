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
        History history;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        int interrupt_mask;

        // If you return this object, the stack will unwind.
        // You should not return this object from a primitive.   
        public static readonly object [] UnwindStack = new object [] { };
        static readonly SCode UnwindStackExpression = Quotation.Make (UnwindStack);

        public Interpreter ()
        { }

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
        public static bool UserApply (out object answer, object arg0, object arg1)
        {
            ////Primitive.Noisy = true;
            IApplicable op = arg0 as IApplicable;
            Cons rands = arg1 as Cons;
            if (op == null) throw new NotImplementedException ("Apply non applicable.");
            if (arg1 != null && rands == null) throw new NotImplementedException ("Bad list to apply.");
            answer = new TailCallInterpreter (new ApplyFromPrimitive (op, new Cons (op, rands)), null);
            return true; // special return
        }

        [SchemePrimitive ("CALL-WITH-CURRENT-CONTINUATION", 1)]
        public static bool CallWithCurrentContinuation (out object answer, object arg)
        {
            // extreme hair ahead

            IApplicable receiver = arg as IApplicable;
            if (receiver == null) throw new NotImplementedException ("Receiver cannot be applied.");
            // Create an initial unwinder state.  After reload, we resume at
            // CWCCFrame0.
            UnwinderState env = new UnwinderState (new CWCCFrame0 (receiver));

            // Return from the primitive with instructions to unwind the stack.
            answer = new TailCallInterpreter (Interpreter.UnwindStackExpression, env);
            return true;
        }

        [SchemePrimitive ("WITHIN-CONTROL-POINT", 2)]
        public static bool WithinControlPoint (out object answer, object arg0, object arg1)
        {
            // extreme hair ahead

            IApplicable thunk = arg1 as IApplicable;
            if (thunk == null) throw new NotImplementedException ("Thunk is not applicable.");

            // Create an initial unwinder state.  After reload, we resume at
            // WithinControlPointFrame.
            UnwinderState env = new UnwinderState ((ControlPoint) arg0, new WithinControlPointFrame (thunk));

            // Return from the primitive with instructions to unwind the stack.
            answer = new TailCallInterpreter (Interpreter.UnwindStackExpression, env);
            return true;
        }

        [SchemePrimitive ("GET-INTERRUPT-ENABLES", 0)]
        public static bool GetInterruptEnables (out object answer)
        {
            answer = 0;
            return false;
        }

        [SchemePrimitive ("SCODE-EVAL", 2)]
        public static bool ScodeEval (out object answer, object arg0, object arg1)
        {
            Environment env = Environment.ToEnvironment (arg1);
            CompileTimeEnvironment ctenv = new CompileTimeEnvironment (null);
            //CompileTimeEnvironment ctenv = (env is TopLevelEnvironment)
            //    ? new CompileTimeEnvironment (((TopLevelEnvironment) env).Closure.Lambda.Formals)
            //    : new CompileTimeEnvironment (null);
            SCode sarg0 = SCode.EnsureSCode (arg0).Bind (ctenv);
            answer = new TailCallInterpreter (sarg0, env);
            return true;
        }

        [SchemePrimitive ("SET-INTERRUPT-ENABLES!", 1)]
        public static bool SetInterruptEnables (out object answer, object arg)
        {
            answer = null;
            return false;
        }

        [SchemePrimitive ("WITH-HISTORY-DISABLED", 1)]
        public static bool WithHistoryDisabled (out object answer, object arg0)
        {
            IApplicable thunk = arg0 as IApplicable;
            if (thunk == null) throw new NotImplementedException ("Thunk is not applicable.");
            answer = new TailCallInterpreter (new HistoryDisabled (thunk), null);
            return true;
        }

        [SchemePrimitive ("WITH-INTERRUPT-MASK", 2)]
        public static bool WithInterruptMask (out object answer, object arg0, object arg1)
        {
            IApplicable receiver = arg1 as IApplicable;
            if (receiver == null) throw new NotImplementedException ("Receiver is not applicable.");
            answer = new TailCallInterpreter (new InterruptMask (arg0, receiver), null);
            return true;
        }

        [SchemePrimitive ("WITH-STACK-MARKER", 3)]
        public static bool WithStackMarker (out object answer, object thunk, object mark1, object mark2)
        {
            IApplicable athunk = thunk as IApplicable;
            if (athunk == null) throw new NotImplementedException ("Thunk is not applicable.");
            answer = new TailCallInterpreter (new StackMarker (athunk, mark1, mark2), null);
            return true;
        }

        internal static bool Apply (out object answer, ref SCode expression, ref Environment environment, object evop, object [] evargs)
        {
            IApplicable op = evop as IApplicable;
            if (op == null) throw new NotImplementedException ("Application of non-procedure object.");
            return op.Apply (out answer, ref expression, ref environment, evargs);
        }

        internal static bool Call (out object answer, ref SCode expression, ref Environment environment, object evop)
        {
            IApplicable op = evop as IApplicable;
            if (op == null) throw new NotImplementedException ("Application of non-procedure object.");
            return op.Call (out answer, ref expression, ref environment);
        }

        internal static bool Call (out object answer, ref SCode expression, ref Environment environment, object evop, object evarg)
        {
            IApplicable op = evop as IApplicable;
            if (op == null) throw new NotImplementedException ("Application of non-procedure object.");
            return op.Call (out answer, ref expression, ref environment, evarg);
        }

        internal static bool Call (out object answer, ref SCode expression, ref Environment environment, object evop, object evarg0, object evarg1)
        {
            IApplicable op = evop as IApplicable;
            if (op == null) throw new NotImplementedException ("Application of non-procedure object.");
            return op.Call (out answer, ref expression, ref environment, evarg0, evarg1);
        }
    }

    /// <summary>
    /// Primitives return one of these objects in order to get
    /// the interpreter to tail call.
    /// </summary>
    sealed class TailCallInterpreter
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly SCode expression;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly Environment environment;

        public TailCallInterpreter (SCode expression, Environment environment)
        {
            this.expression = expression;
            this.environment = environment;
        }

        public SCode Expression
        {
            [DebuggerStepThrough]
            get
            {
                return this.expression;
            }
        }

        public Environment Environment
        {
            [DebuggerStepThrough]
            get
            {
                return this.environment;
            }
        }
    }

    sealed class CWCCFrame0 : ContinuationFrame
    {
        IApplicable receiver;

        public CWCCFrame0 (IApplicable receiver)
        {
            this.receiver = receiver;
        }

        public override bool EvalStep (out object answer, ref SCode expression, ref Environment environment)
        {
            return this.receiver.Call (out answer, ref expression, ref environment, ((RewindState) environment).ControlPoint);
        }
    }

    sealed class WithinControlPointFrame : ContinuationFrame
    {
        IApplicable thunk;

        public WithinControlPointFrame (IApplicable thunk)
        {
            this.thunk = thunk;
        }

        public override bool EvalStep (out object answer, ref SCode expression, ref Environment environment)
        {
            return this.thunk.Call (out answer, ref expression, ref environment);
        }
    }

    sealed class ApplyFromPrimitive : SCode
    {
        internal IApplicable op;
        internal Cons rands;

        internal ApplyFromPrimitive (IApplicable op, Cons rands)
            : base (TC.SPECIAL)
        {
            this.op = op;
            this.rands = rands;
        }

        public override SCode Bind (CompileTimeEnvironment ctenv)
        {
            throw new NotImplementedException ();
        }

        public override HashSet<string> FreeVariables ()
        {
            throw new NotImplementedException ();
        }
        
        public override bool EvalStep (out object answer, ref SCode expression, ref Environment environment)
        {
            return this.op.Apply (out answer, ref expression, ref environment, this.rands.ToVector ());
        }

        public override bool NeedsTheEnvironment ()
        {
            throw new NotImplementedException ();
        }
    }

    sealed class HistoryDisabled : SCode
    {
        IApplicable thunk;

        internal HistoryDisabled (IApplicable arg0)
            : base (TC.SPECIAL)
        {
            this.thunk = arg0;
        }

        public override SCode Bind (CompileTimeEnvironment ctenv)
        {
            throw new NotImplementedException ();
        }

        public override HashSet<string> FreeVariables ()
        {
            throw new NotImplementedException ();
        }
        
        public override bool EvalStep (out object answer, ref SCode expression, ref Environment environment)
        {
            object returnValue = null;
            SCode expr = null;
            Environment env = null;
            if (this.thunk.Call (out returnValue, ref expr, ref env)) {
                while (expr.EvalStep (out returnValue, ref expr, ref env)) { };
            }
            if (returnValue == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new HistoryDisabledFrame (this));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }
            // re-enable history here
            answer = returnValue;
            return false;
        }

        public override bool NeedsTheEnvironment ()
        {
            throw new NotImplementedException ();
        }
    }

    sealed class HistoryDisabledFrame : ContinuationFrame, ISystemVector
    {
        HistoryDisabled historyDisabled;

        public HistoryDisabledFrame (HistoryDisabled historyDisabled)
        {
            this.historyDisabled = historyDisabled;
        }

        public override bool EvalStep (out object answer, ref SCode expression, ref Environment environment)
        {
            SCode expr = ((RewindState) environment).PopFrame ();
            Environment env = environment;
            while (expr.EvalStep (out answer, ref expr, ref env)) { };
            if (answer == Interpreter.UnwindStack) {
                ((UnwinderState) env).AppendContinuationFrames (this.continuation);
                //((UnwinderState) env).AppendContinuationFrames ((RewindState) environment.OldFrames);
                environment = env;
                return false;
            }

            return false;
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

    sealed class InterruptMask : SCode
    {
        object arg0;
        IApplicable receiver;

        internal InterruptMask (object arg0, IApplicable receiver)
            : base (TC.SPECIAL)
        {
            this.arg0 = arg0;
            this.receiver = receiver;
        }

        public override SCode Bind (CompileTimeEnvironment ctenv)
        {
            throw new NotImplementedException ();
        }

        public override bool EvalStep (out object answer, ref SCode expression, ref Environment environment)
        {
            int oldMask = 0;
            object returnValue = null;
            SCode expr = null;
            Environment env = null;
            if (this.receiver.Call (out returnValue, ref expr, ref env, oldMask)) {
                while (expr.EvalStep (out returnValue, ref expr, ref env)) { };
            }
            if (returnValue == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new InterruptMaskFrame (this, oldMask));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }
            // restore mask here
            answer = returnValue;
            return false;
        }

        public override HashSet<string> FreeVariables ()
        {
            throw new NotImplementedException ();
        }

        public override bool NeedsTheEnvironment ()
        {
            throw new NotImplementedException ();
        }


    }

    sealed class StackMarker : SCode
    {
        IApplicable thunk;
        object mark1;
        object mark2;

        internal StackMarker (IApplicable thunk, object mark1, object mark2)
            : base (TC.SPECIAL)
        {
            this.thunk = thunk;
            this.mark1 = mark1;
            this.mark2 = mark2;
        }

        public override SCode Bind (CompileTimeEnvironment ctenv)
        {
            throw new NotImplementedException ();
        }

        public override bool EvalStep (out object answer, ref SCode expression, ref Environment environment)
        {
            object returnValue = null;
            SCode expr = null;
            Environment env = null;
            if (this.thunk.Call (out returnValue, ref expr, ref env)) {
                while (expr.EvalStep (out returnValue, ref expr, ref env)) { };
            }
            if (returnValue == Interpreter.UnwindStack) {
                ((UnwinderState) env).AddFrame (new StackMarkerFrame (this));
                answer = Interpreter.UnwindStack;
                environment = env;
                return false;
            }
            answer = returnValue;
            return false;
        }

        public override HashSet<string> FreeVariables ()
        {
            throw new NotImplementedException ();
        }

        public override bool NeedsTheEnvironment ()
        {
            throw new NotImplementedException ();
        }
    }

    sealed class StackMarkerFrame : ContinuationFrame, ISystemVector
    {
        StackMarker stackMarker;

        internal StackMarkerFrame (StackMarker stackMarker)
        {
            this.stackMarker = stackMarker;
        }

        public override bool EvalStep (out object answer, ref SCode expression, ref Environment environment)
        {
            SCode expr = ((RewindState) environment).PopFrame ();
            Environment env = environment;
            while (expr.EvalStep (out answer, ref expr, ref env)) { };
            if (answer == Interpreter.UnwindStack) {
                ((UnwinderState) env).AppendContinuationFrames (this.continuation);
                //((UnwinderState) env).AppendContinuationFrames ((RewindState) environment.OldFrames);
                environment = env;
                return false;
            }
         
            return false;
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

    sealed class InterruptMaskFrame : ContinuationFrame, ISystemVector
    {
        InterruptMask interruptMask;

        internal InterruptMaskFrame (InterruptMask interruptMask, int oldMask)
        {
            this.interruptMask = interruptMask;
        }

        public override bool EvalStep (out object answer, ref SCode tailCall, ref Environment environment)
        {
            SCode expr = ((RewindState) environment).PopFrame ();
            Environment env = environment;
            while (expr.EvalStep (out answer, ref expr, ref env)) { };
            if (answer == Interpreter.UnwindStack) {
                ((UnwinderState) env).AppendContinuationFrames (this.continuation);
                //((UnwinderState) env).AppendContinuationFrames ((RewindState) environment.OldFrames);
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }

// restore mask
            return false;
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
}
