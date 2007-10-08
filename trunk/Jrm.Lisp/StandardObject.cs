using System;
using System.Diagnostics;

namespace Lisp
{
    [DebuggerDisplay ("{((ManifestInstance)Target).ObjectDebuggerDisplay, nq}")]
    /// <summary>Represents a CLOS object.</summary>
    public delegate object StandardObject (params object [] arguments);

    delegate object ApplyHandler (StandardObject self, object [] arguments);

    class UnboundSlot
    {
        static UnboundSlot theUnboundSlot;
        private UnboundSlot ()
        {
        }

        static public UnboundSlot Value
        {
            get
            {
                if (theUnboundSlot == null)
                    theUnboundSlot = new UnboundSlot ();
                return theUnboundSlot;
            }
        }
    }


    [DebuggerDisplay ("{InstanceDebuggerDisplay, nq}")]
    /// <summary>Represents the state of a CLOS object.</summary>
    /// <remarks>ManifestInstance objects should not be used or
    /// manipulated by code outside of the CLOS class.  External code
    /// should always be using a StandardObject delegate.</remarks>
    internal class ManifestInstance
    {
        static int nextSerialNumber;
        static object UnboundSlotValue = UnboundSlot.Value;

        readonly int serialNumber;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        StandardObject self;

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
                return this.serialNumber;
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
            get
            {
                return this.onApply;
            }
            [DebuggerStepThrough]
            set
            {
                this.onApply = value;
            }
        }

        [DebuggerStepThrough]
        object DefaultInstanceMethod (params object [] arguments)
        {
            if (onApply == null)
                throw new NotImplementedException ("Attempt to call " + this.ToString () + " on " + arguments.ToString ());
            else {
                return this.onApply (this.self, arguments);
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
                Symbol className = (Symbol) CLOS.StandardObjectName (this.closClass);
                Symbol instanceName = (Symbol) CLOS.StandardObjectName (this.self);
                string classInfo = (className == null) ? "StandardObject" : className.Name;
                string info = (instanceName == null) ? "" : instanceName.Name;
                return "{" + classInfo + " " + this.serialNumber + " " + info + "}";
            }
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        /// <summary>Debugging helper property.</summary>
        public string InstanceDebuggerDisplay
        {
            get
            {
                return "{ManifestInstance " + this.serialNumber + " of " + ((Symbol) CLOS.ClassName (this.closClass)).Name + "}";
            }
        }

        static object applyStandardObject (StandardObject self, object [] arguments)
        {
            throw new NotImplementedException ("Attempt to apply non function " + self + " to " + arguments.ToString ());
        }

        static object applyUninitializedObject (StandardObject self, object [] arguments)
        {
            throw new NotImplementedException (self.ToString () + ": Attempt to apply uninitialized " + ((Symbol) CL.ClassName (((ManifestInstance) self.Target).Class)).Name + " to " + arguments.ToString ());
        }

        static StandardObject CreateInstance (StandardObject closClass, int nSlots, ApplyHandler method)
        {
            object [] slotVector = new object [nSlots];
            for (int i = 0; i < nSlots; i++)
                slotVector [i] = UnboundSlotValue;
            StandardObject answer =
                (StandardObject) Delegate.CreateDelegate (typeof (StandardObject),
                                                          new ManifestInstance (closClass, slotVector, method),
                                                          typeof (ManifestInstance)
                                                             .GetMethod ("DefaultInstanceMethod",
                                                                         System.Reflection.BindingFlags.Instance
                                                                         | System.Reflection.BindingFlags.NonPublic));
            ((ManifestInstance) answer.Target).self = answer;
            return answer;
        }

        public static StandardObject CreateInstance (StandardObject closClass, int nSlots)
        {
            return CreateInstance (closClass, nSlots, applyStandardObject);
        }

        public static StandardObject CreateFuncallableInstance (StandardObject closClass, int nSlots)
        {
            return CreateInstance (closClass, nSlots, applyUninitializedObject);
        }

        public static StandardObject CreateFuncallableInstance (StandardObject closClass, int nSlots, ApplyHandler applyHandler)
        {
            return CreateInstance (closClass, nSlots, applyHandler);
        }
    }
}
