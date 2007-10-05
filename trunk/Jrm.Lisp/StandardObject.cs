using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Lisp
{
    [System.Diagnostics.DebuggerDisplay ("{((ManifestInstance)Target).ObjectDebuggerDisplay, nq}")]
    public delegate object StandardObject (params object [] arguments);

    delegate object ApplyHandler (ManifestInstance self, object [] arguments);

    [System.Diagnostics.DebuggerDisplay ("{InstanceDebuggerDisplay, nq}")]
    class ManifestInstance
    {
        static int nextSerialNumber;

        [System.Diagnostics.DebuggerBrowsable (System.Diagnostics.DebuggerBrowsableState.Never)]
        int serialNumber;

        [System.Diagnostics.DebuggerBrowsable (System.Diagnostics.DebuggerBrowsableState.Never)]
        StandardObject closClass;

        [System.Diagnostics.DebuggerBrowsable (System.Diagnostics.DebuggerBrowsableState.Never)]
        object [] slotVector;

        [System.Diagnostics.DebuggerBrowsable (System.Diagnostics.DebuggerBrowsableState.Never)]
        ApplyHandler onApply;

        public ManifestInstance (StandardObject closClass, object [] slotVector, ApplyHandler onApply)
        {
            this.serialNumber = nextSerialNumber++;
            this.closClass = closClass;
            this.slotVector = slotVector;
            this.onApply = onApply;
        }

        public StandardObject Class
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return this.closClass;
            }
            [System.Diagnostics.DebuggerStepThrough]
            set
            {
                this.closClass = value;
            }
        }

        public int SerialNumber
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return this.SerialNumber;
            }
        }

        public object [] Slots
        {
            [System.Diagnostics.DebuggerStepThrough]
            get
            {
                return this.slotVector;
            }
            [System.Diagnostics.DebuggerStepThrough]
            set
            {
                this.slotVector = value;
            }
        }

        public ApplyHandler OnApply
        {
            [System.Diagnostics.DebuggerStepThrough]
            set
            {
                this.onApply = value;
            }
        }

        object DefaultInstanceMethod (params object [] arguments)
        {
            if (onApply == null)
                throw new NotImplementedException ("Attempt to call " + this.ToString1 () + " on " + arguments.ToString ());
            else {
                return this.onApply (this, arguments);
            }
        }

        public string ToString1 ()
        {
            return "mi.tostring1";
        }

        public override string ToString ()
        {
            return "{ManifestInstance " + this.serialNumber + "}";
        }

        public string ObjectDebuggerDisplay
        {
            get
            {
                return "{StandardObject " + this.serialNumber + "}";
            }
        }

        public string InstanceDebuggerDisplay
        {
            get
            {
                return "{ManifestInstance " + this.serialNumber + "}";
            }
        }

    }

}
