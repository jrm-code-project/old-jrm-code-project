using System;
using System.Collections.Generic;
using System.Diagnostics;

namespace Microcode
{
    public abstract class ContinuationFrame : SCode
    {
        public ContinuationFrameList continuation;

        public ContinuationFrame ()
            : base (TC.CONTROL_POINT)
        {
        }

        public override SCode Bind (CompileTimeEnvironment ctenv)
        {
            throw new NotImplementedException ();
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

    public abstract class SubproblemContinuation<T> : ContinuationFrame where T : SCode
    {
        protected T expression;
        protected Environment environment;

        public SubproblemContinuation (T expression, Environment environment)
        {
            this.expression = expression;
            this.environment = environment;
        }

        public override bool EvalStep (out object answer, ref SCode expression, ref Environment environment)
        {
            object temp;
            SCode expr = ((RewindState) environment).PopFrame ();
            Environment env = environment;
            while (expr.EvalStep (out temp, ref expr, ref env)) { };
            if (temp == Interpreter.UnwindStack) {
                ((UnwinderState) env).AppendContinuationFrames (this.continuation);
                environment = env;
                answer = Interpreter.UnwindStack;
                return false;
            }
            expression = this.expression;
            environment = this.environment;
            return Continue (out answer, ref expression, ref environment, temp);
        }

        public abstract bool Continue (out object answer, ref SCode expression, ref Environment environment, object value);
    }


    public class Continuation : SchemeObject
    {

        public Continuation (ContinuationFrameList new_frames, ContinuationFrameList old_frames)
            : base (TC.CONTROL_POINT)
        {
        }

        static public object Initial (SCode expression, Environment environment)
        {
            object answer;
            bool bounce;
            do {
                answer = null;
                bounce = false;
                while (expression.EvalStep (out answer, ref expression, ref environment)) { };
                if (answer == Interpreter.UnwindStack) {
                    // What are we doing here?  Someone unwound the stack!
                    // There are two possibilities to consider:
                    //  1) A cwcc has just performed a destructive read of the stack.
                    //     In this case, we convert the newly captured frame list
                    //     into a control point and reload it.
                    //
                    //  2) A within-continuation has blown away the stack in order
                    //     to install a different control point.  In this case we
                    //     reload the control point that was stored in the UnwinderState.
                    ControlPoint stateToRestore = ((UnwinderState) environment).ToControlPoint ();

                    // the receiver gets control when the stack is put back.
                    SCode receiver = ((UnwinderState) environment).Receiver;

                    // the rewind state goes in the environment
                    environment = new RewindState (stateToRestore, receiver);

                    // Start reloading by returning control to the lowest
                    // frame.
                    expression = ((RewindState) environment).PopFrame ();
                    bounce = true;
                }
            } while (bounce);
            return answer;
        }
    }

    //abstract class Subproblem<T> : Continuation
    //{
    //    [DebuggerBrowsable (DebuggerBrowsableState.Never)]
    //    readonly T expression;

    //    [DebuggerBrowsable (DebuggerBrowsableState.Never)]
    //    readonly Environment environment;

    //    [DebuggerStepThrough]
    //    public Subproblem (Continuation next, T expression, Environment environment)
    //        : base (next)
    //    {
    //        this.expression = expression;
    //        this.environment = environment;
    //    }

    //    public T Expression
    //    {
    //        [DebuggerStepThrough]
    //        get
    //        {
    //            return this.expression;
    //        }
    //    }

    //    public Environment Environment
    //    {
    //        [DebuggerStepThrough]
    //        get
    //        {
    //            return this.environment;
    //        }
    //    }
    
}
