using System;
using System.Diagnostics;

namespace Microcode
{
    class ValueCell
    {
        //static object unassigned = Constant.Unassigned;

        object val = ReferenceTrap.Unassigned;

        public ValueCell ()
        {
        }

        public ValueCell (object initialValue)
        {
            this.val = initialValue;
        }

        public object Assign (object newValue)
        {
            object oldValue = this.val;
            this.val = newValue;
            return oldValue;
        }

        public object Value
        {
            get
            {
                        return this.val;
                }
        }
    }
}
