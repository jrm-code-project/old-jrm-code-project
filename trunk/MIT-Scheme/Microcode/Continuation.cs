using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Threading;

namespace Microcode
{
    [Serializable]
    public abstract class ContinuationFrame : Control
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public override TC TypeCode { get { return TC.CONTROL_POINT; } }

        public ContinuationFrameList continuation;

        [DebuggerStepThrough]
        protected ContinuationFrame ()
            : base ()
        {
        }
    }

    [Serializable]
    public abstract class SubproblemContinuation<T> : ContinuationFrame where T : SCode
    {
        protected T expression;
        protected Environment environment;

        protected SubproblemContinuation (T expression, Environment environment)
        {
            this.expression = expression;
            this.environment = environment;
        }

        public abstract bool Continue (out object answer, ref Control expression, ref Environment environment, object value);

        public override bool EvalStep (out object answer, ref Control expression, ref Environment environment)
        {
            object temp;
            Control expr = ((RewindState) environment).PopFrame ();
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
    }

    public class Continuation : SchemeObject
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public override TC TypeCode { get { return TC.CONTROL_POINT; } }

        public Continuation (ContinuationFrameList new_frames, ContinuationFrameList old_frames)
        {
        }

        static public object Initial (WorldState stateToRestore)
        {
            throw new NotImplementedException ();
            //Environment.Global = stateToRestore.ge;
            //Environment closureEnvironment = new RewindState (stateToRestore.cp, new RestoreBandFrame ());
            //Control expression = ((RewindState) closureEnvironment).PopFrame ();
            //return Initial (expression, closureEnvironment);
        }

        static public object Initial (SCode expression, Environment environment)
        {
            object answer;
            bool bounce;
#if DEBUG
            Timer tosProbe = new Timer (SCode.TopOfStackProbe, null, 5, 3);
            SCode.topOfStackTimer = tosProbe;
#endif
            Control residual = expression.PartialEval (environment).Residual;
            do {
                answer = null;
                bounce = false;
                while (residual.EvalStep (out answer, ref residual, ref environment)) { };
                if (answer == Interpreter.UnwindStack) {
                    // What are we doing here?  Someone unwound the stack!
                    // There are three possibilities to consider:
                    //  1) A cwcc has just performed a destructive read of the stack.
                    //     In this case, we convert the newly captured frame list
                    //     into a control point and reload it.
                    //
                    //  2) A within-continuation has blown away the stack in order
                    //     to install a different control point.  In this case we
                    //     reload the control point that was stored in the UnwinderState.
                    //
                    //  3) The stack was unwound in order to exit the interpreter.
                    //     In this case, we return from the initial continuation.
                    if (((UnwinderState) environment).IsExit)
                        return ((UnwinderState) environment).ExitValue;
                    ControlPoint stateToRestore = ((UnwinderState) environment).ToControlPoint ();

                    // the receiver gets control when the stack is put back.
                    Control receiver = ((UnwinderState) environment).Receiver;

                    // the rewind state goes in the closureEnvironment
                    environment = new RewindState (stateToRestore, receiver);

                    // Start reloading by returning control to the lowest
                    // frame.
                    residual = ((RewindState) environment).PopFrame ();
                    bounce = true;
                }
            } while (bounce);
            return answer;
        }
    }
    
}
