using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    public class ContinuationFrameList : ISystemVector
    {

        const int ExtraFakeSlots = 2;
        public ContinuationFrame first;
        public ContinuationFrameList rest;

        public ContinuationFrameList (ContinuationFrame first, ContinuationFrameList rest)
        {
            this.first = first;
            this.rest = rest;
        }

        public static ContinuationFrameList reverse (ContinuationFrameList original)
        {
            ContinuationFrameList result = null;
            while (original != null) {
                result = new ContinuationFrameList (original.first, result);
                original = original.rest;
            }
            return result;
        }

        #region ISystemVector Members

        public int SystemVectorSize
        {
            get { return ((ISystemVector)first).SystemVectorSize 
                           // two more slots at the end
                           + (rest == null ? ExtraFakeSlots : rest.SystemVectorSize); }
        }

        public object SystemVectorRef (int index)
        {
            ISystemVector firstVec = ((ISystemVector) first);
            int firstSize = firstVec.SystemVectorSize;
            return (index < firstSize) ? firstVec.SystemVectorRef (index)
                : (rest == null) ? Fakeup (index)
                : rest.SystemVectorRef (index - firstSize)
                 ;
        }

        public object Fakeup (int index)
        {
            // no stacklets
            if (index == 3) return ReturnCode.HALT;
            throw new NotImplementedException ();
        }

        public object SystemVectorSet (int index, object newValue)
        {
            throw new NotImplementedException ();
        }

        #endregion
    }
  
}
