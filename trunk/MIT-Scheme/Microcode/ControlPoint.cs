using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    class ControlPoint : ISystemVector
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

        int interruptMask;
        History history;
        Continuation continuation;
        public ControlPoint (int interruptMask, History history, Continuation continuation)
        {
            this.interruptMask = interruptMask;
            this.history = history;
            this.continuation = continuation;
        }

        public object Restore (Interpreter interpreter, SCode thunk)
        {
            interpreter.Continuation = this.continuation;
            return interpreter.Apply (thunk, new object [] { });
        }

        #region ISystemVector Members

        public int SystemVectorSize
        {
            get { return this.continuation.SystemVectorSize + 8; }
        }

        public object SystemVectorRef (int index)
        {
            if (index == 1)
                return 0; // unused length
            else if (index == 3)
                return this.interruptMask;
            else if (index == 5)
                return this.history;
            else if (index == 6)
                return 0;
            else if (index == 7)
                return false;
            else if (index < 8)
                throw new NotImplementedException ();
            else
                return this.continuation.SystemVectorRef (index - 8);
        }

        public object SystemVectorSet (int index, object newValue)
        {
            throw new NotImplementedException ();
        }

        #endregion

        [SchemePrimitive ("WITHIN-CONTROL-POINT", 2)]
        public static object WithinControlPoint (Interpreter interpreter, object arg0, object arg1)
        {
            return ((ControlPoint) (arg0)).Restore (interpreter, (SCode) (arg1));
        }
    }
}
