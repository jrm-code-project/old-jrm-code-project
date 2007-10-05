using System;
using System.Diagnostics;

namespace Lisp
{
    [DebuggerDisplay ("{((ManifestInstance)Target).ObjectDebuggerDisplay, nq}")]
    /// <summary>Represents a CLOS object.</summary>
    public delegate object StandardObject (params object [] arguments);

    delegate object ApplyHandler (ManifestInstance self, object [] arguments);

    [DebuggerDisplay ("{InstanceDebuggerDisplay, nq}")]
    /// <summary>Represents the state of a CLOS object.</summary>
    /// <remarks>ManifestInstance objects should not be used or
    /// manipulated by code outside of the CLOS class.  External code
    /// should always be using a StandardObject delegate.</remarks>
    internal class ManifestInstance
    {
        static int nextSerialNumber;

        readonly int serialNumber;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        StandardObject closClass;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        object [] slotVector;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
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
            [DebuggerStepThrough]
            get
            {
                return this.closClass;
            }
            [DebuggerStepThrough]
            set
            {
                this.closClass = value;
            }
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public int SerialNumber
        {
            [DebuggerStepThrough]
            get
            {
                return this.SerialNumber;
            }
        }

        public object [] Slots
        {
            [DebuggerStepThrough]
            get
            {
                return this.slotVector;
            }
            [DebuggerStepThrough]
            set
            {
                this.slotVector = value;
            }
        }

        public ApplyHandler OnApply
        {
            [DebuggerStepThrough]
            set
            {
                this.onApply = value;
            }
        }

        object DefaultInstanceMethod (params object [] arguments)
        {
            if (onApply == null)
                throw new NotImplementedException ("Attempt to call " + this.ToString () + " on " + arguments.ToString ());
            else {
                return this.onApply (this, arguments);
            }
        }

        // We shouldn't try to use the ToString method
        // in `real' code, so we don't override it here.
        //public override string ToString ()
        //{
        //    return "{ManifestInstance " + this.serialNumber + "}";
        //}


        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        ///<summary>Debugging helper property.</summary>
        public string ObjectDebuggerDisplay
        {
            get
            {
                return "{StandardObject " + this.serialNumber + "}";
            }
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        /// <summary>Debugging helper property.</summary>
        public string InstanceDebuggerDisplay
        {
            get
            {
                return "{ManifestInstance " + this.serialNumber + "}";
            }
        }
    }
}
