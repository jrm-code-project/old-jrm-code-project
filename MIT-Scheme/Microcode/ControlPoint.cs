using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    [Serializable]
    public class ControlPoint : SchemeObject, ISystemVector
    {
        // 0 reusable?
        // 1 unused-length
        // 2   <restore-interrupt-mask return-address>
        // 3 interrupt-mask
        // 4   <restore-history return-address>
        // 5 history
        // 6 previous-history-offset
        // 7 previous-history-control-point
        // 8 first-element-index

        /// <summary>
        /// Holds the list of frames in order from most recent to least recent.
        /// This is the direction we want for sharing, but the reverse direction
        /// for loading.
        /// </summary>
        ContinuationFrameList frames;

        //int interruptMask;
        //History history;
        //Continuation continuation;
        //public ControlPoint (int interruptMask, History history, Continuation continuation)
        //{
        //    throw new NotImplementedException ();
        //    //this.interruptMask = interruptMask;
        //    //this.history = history;
        //    //this.continuation = continuation;
        //}

        public ContinuationFrameList FrameList
        {
            get
            {
                return this.frames;
            }
        }

        public ControlPoint (ContinuationFrameList newFrames, ContinuationFrameList oldFrames)
            : base (TC.CONTROL_POINT)
        {
            // The new frames don't know what the continuation is below them.
            // We take them one by one and push them onto the old_frames
            // while setting their continuation to the list of frames below.
            frames = oldFrames;
            while (newFrames != null) {
                ContinuationFrame new_frame = newFrames.first;
                newFrames = newFrames.rest;
                if (new_frame.continuation != null)
                    throw new Exception ("Continuation not empty?");
                new_frame.continuation = frames;
                frames = new ContinuationFrameList (new_frame, frames);
            }
        }

        public object Restore (Interpreter interpreter, SCode thunk)
        {
            throw new NotImplementedException ();
            //interpreter.Continuation = this.continuation;
            //return interpreter.Apply (thunk, new object [] { });
        }

        #region ISystemVector Members

        public int SystemVectorSize
        {
            get
            {
                return this.frames.SystemVectorSize + 8;
            }
        }

        public object SystemVectorRef (int index)
        {
            if (index == 1)
                return 0; // unused length
            else if (index == 3)
                //throw new NotImplementedException ();
                return 0;
            //return this.interruptMask;
            else if (index == 5)
                //throw new NotImplementedException();
                return History.DummyHistory ();
            //return this.history;
            else if (index == 6)
                return 0;
            else if (index == 7)
                return false;
            else if (index < 8)
                throw new NotImplementedException ();
            else
                return this.frames.SystemVectorRef (index - 8);
        }

        public object SystemVectorSet (int index, object newValue)
        {
            throw new NotImplementedException ();
        }

        #endregion

        //public override bool EvalStep (out object answer, ref SCode expression, ref Environment environment)
        //{
        //    throw new NotImplementedException ();
        //}

        //public override SCode Bind (CompileTimeEnvironment ctenv)
        //{
        //    throw new NotImplementedException ();
        //}
    }
}
