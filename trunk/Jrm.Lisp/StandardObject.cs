using System;
using System.Diagnostics;
using System.Reflection;

namespace Lisp
{
    [DebuggerDisplay ("{((ManifestInstance)Target).ObjectDebuggerDisplay, nq}")]
    /// <summary>Represents a CLOS object.</summary>
    public delegate object StandardInstance (params object [] arguments);

    delegate object FuncallHandler (StandardInstance self, object [] arguments);

    internal class FuncallHandlerWrapper
    {
        Delegate del;

        FuncallHandlerWrapper (Delegate del) {
            this.del = del;
        }

        static bool IsRestArg (ParameterInfo parameter)
        {
            return parameter.GetCustomAttributes (typeof (System.ParamArrayAttribute), false).Length > 0;
        }

        object [] PrePend (object element, object [] arguments)
        {
            object [] answer = new object [arguments.Length + 1];
            arguments.CopyTo (answer, 1);
            answer [0] = element;
            return answer;
        }

        object Funcall (StandardInstance self, object [] arguments)
        {
            return del.DynamicInvoke (PrePend (self, arguments));
        }

        static MethodInfo FuncallMethod =
            typeof (FuncallHandlerWrapper)
              .GetMethod ("Funcall", BindingFlags.NonPublic | BindingFlags.Instance);

        public static FuncallHandler Create (Delegate del)
        {
            return (FuncallHandler) 
                Delegate.CreateDelegate (typeof (FuncallHandler),
                                            new FuncallHandlerWrapper (del),
                                            FuncallMethod
                                            );
        }

        public static FuncallHandler Create (MethodInfo method)
        {
            ParameterInfo [] parameters = method.GetParameters ();
            if (parameters.Length == 2
                && parameters [0].ParameterType == typeof (StandardInstance)
                && IsRestArg (parameters[1]))
                throw new NotImplementedException ("Working create");
            throw new NotImplementedException ("failing create");
        }

    }

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
    /// should always be using a StandardInstance delegate.</remarks>
    internal class ManifestInstance
    {
        static int nextSerialNumber;
        static object UnboundSlotValue = UnboundSlot.Value;

        readonly int serialNumber;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        StandardInstance self;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        StandardInstance closClass;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        object [] slotVector;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        FuncallHandler onFuncall;

        public ManifestInstance (StandardInstance closClass, object [] slotVector, FuncallHandler onFuncall)
        {
            this.serialNumber = nextSerialNumber++;
            this.closClass = closClass;
            this.slotVector = slotVector;
            this.onFuncall = onFuncall;
        }

        public StandardInstance Class
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

        public FuncallHandler OnFuncall
        {
            [DebuggerStepThrough]
            get
            {
                return this.onFuncall;
            }
            [DebuggerStepThrough]
            set
            {
                if (value == null)
                    throw new ArgumentNullException ("value");
                this.onFuncall = value;
            }
        }

        [DebuggerStepThrough]
        object DefaultInstanceMethod (params object [] arguments)
        {
            if (onFuncall == null)
                throw new NotImplementedException ("Attempt to call " + this.ToString () + " on " + arguments.ToString ());
            else {
                return this.onFuncall (this.self, arguments);
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
                object className = CLOS.StandardObjectName (this.closClass);
                string classNameString = (className is string)
                    ? (string) className
                    : (className is Symbol)
                    ? ((Symbol) className).Name
                    : "StandardInstance";
                object instanceName = CLOS.StandardObjectName (this.self);
                string instanceNameString = (instanceName is string)
            ? (string) instanceName
            : (instanceName is Symbol)
                ? ((Symbol) instanceName).Name
                : "";

                return "{" + classNameString + " " + this.serialNumber + " " + instanceName + "}";
            }
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        /// <summary>Debugging helper property.</summary>
        public string InstanceDebuggerDisplay
        {
            get
            {
                return "{ManifestInstance " + this.ObjectDebuggerDisplay +  "}";
            }
        }

        static object funcallStandardObject (StandardInstance self, object [] arguments)
        {
            throw new NotImplementedException ("Attempt to apply non function " + self + " to " + arguments.ToString ());
        }
            
        static object funcallUninitializedObject (StandardInstance self, object [] arguments)
        {
            // throw new NotImplementedException (self.ToString () + ": Attempt to apply uninitialized " + ((Symbol) CL.ClassName (((ManifestInstance) self.Target).Class)).Name + " to " + arguments.ToString ());
            throw new NotImplementedException ();
        }

        static StandardInstance CreateInstance (StandardInstance closClass, object [] slots, FuncallHandler method)
        {

            StandardInstance answer =
                (StandardInstance) Delegate.CreateDelegate (typeof (StandardInstance),
                                                          new ManifestInstance (closClass, slots, method),
                                                          typeof (ManifestInstance)
                                                             .GetMethod ("DefaultInstanceMethod",
                                                                         System.Reflection.BindingFlags.Instance
                                                                         | System.Reflection.BindingFlags.NonPublic));
            ((ManifestInstance) answer.Target).self = answer;
            return answer;
        }

        static StandardInstance CreateInstance (StandardInstance closClass, int nSlots, FuncallHandler method)
        {
            object [] slotVector = null;
            if (nSlots > 0) {
                slotVector = new object [nSlots];
            for (int i = 0; i < nSlots; i++)
                slotVector [i] = UnboundSlotValue;
            }
            return CreateInstance (closClass, nSlots == 0 ? null : slotVector, method);
        }

        public static StandardInstance CreateInstance (StandardInstance closClass, int nSlots)
        {
            return CreateInstance (closClass, nSlots, funcallStandardObject);
        }

        public static StandardInstance CreateInstance (StandardInstance closClass, object [] slots)
        {
            return CreateInstance (closClass, slots, funcallStandardObject);
        }

        public static StandardInstance CreateFuncallableInstance (StandardInstance closClass, int nSlots)
        {
            return CreateInstance (closClass, nSlots, funcallUninitializedObject);
        }

        public static StandardInstance CreateFuncallableInstance (FuncallHandler funcallHandler)
        {
            return CreateInstance (null, 0, funcallHandler);
        }

        public static StandardInstance CreateFuncallableInstance ()
        {
            return CreateInstance (null, 0, funcallUninitializedObject);
        }

        public static StandardInstance CreateInstance ()
        {
            return CreateInstance (null, 0, funcallStandardObject);
        }

        public static StandardInstance CreateFuncallableInstance (StandardInstance closClass, int nSlots, FuncallHandler funcallHandler)
        {
            return CreateInstance (closClass, nSlots, funcallHandler);
        }

        public static StandardInstance CreateFuncallableInstance (StandardInstance closClass, object [] slots, FuncallHandler funcallHandler)
        {
            return CreateInstance (closClass, slots, funcallHandler);
        }
    }
}
