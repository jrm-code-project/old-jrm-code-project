using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Threading;

namespace Microcode
{
    [Serializable]
    public abstract class ContinuationFrame : Control
    {
        public ContinuationFrameList continuation;

        [DebuggerStepThrough]
        protected ContinuationFrame ()
            : base (TC.CONTROL_POINT)
        {
        }
    }

    [Serializable]
    public abstract class SubproblemContinuation<T> : ContinuationFrame where T : SCode
    {
        protected T expression;
        protected Environment environment;

        public SubproblemContinuation (T expression, Environment environment)
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

        public Continuation (ContinuationFrameList new_frames, ContinuationFrameList old_frames)
            : base (TC.CONTROL_POINT)
        {
        }

        static public object Initial (WorldState stateToRestore)
        {
            Constant.theDefaultObject = stateToRestore.defaultObject;
            Constant.theEofObject = stateToRestore.eofObject;
            Constant.theAuxMarker = stateToRestore.aux;
            Constant.theKeyMarker = stateToRestore.key;
            Constant.theOptionalMarker = stateToRestore.optional;
            Constant.theRestMarker = stateToRestore.rest;
            Constant.theExternalUnassignedObject = stateToRestore.externalUnassigned;
            Constant.theUnspecificObject = stateToRestore.unspecific;
            Environment.Global = stateToRestore.ge;
            Environment environment = new RewindState (stateToRestore.cp, new RestoreBandFrame ());
            Control expression = ((RewindState) environment).PopFrame ();
            return Initial (expression, environment);
        }

        static public object Initial (Control expression, Environment environment)
        {
            object answer;
            bool bounce;
#if DEBUG
            Timer tosProbe = new Timer (SCode.TopOfStackProbe, null, 5, 3);
            SCode.topOfStackTimer = tosProbe;
#endif
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
                    Control receiver = ((UnwinderState) environment).Receiver;

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
    
}
