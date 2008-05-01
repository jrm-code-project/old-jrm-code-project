using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    class ValueCell
    {
        static object unassigned = Constant.Unassigned;
        object val;
        public bool isAssigned;

        public ValueCell ()
        {
            this.isAssigned = false;
        }

        public ValueCell (object initialValue)
        {
            this.val = initialValue;
            this.isAssigned = (initialValue != unassigned);
        }

        public object Assign (object newValue)
        {
            object oldValue = this.val;
            this.val = newValue;
            this.isAssigned = (newValue != unassigned);
            return oldValue;
        }

        public object Value
        {
            get
            {
                if (this.isAssigned == true)
                    return this.val;
                else
                    throw new NotImplementedException ();
            }
        }
    }
}
