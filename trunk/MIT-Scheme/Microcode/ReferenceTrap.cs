using System;
using System.Diagnostics;
using System.Runtime.Serialization;
using System.Security.Permissions;

namespace Microcode
{
    enum TrapKind
    {
        NON_TRAP_KIND = 32,
        TRAP_COMPILER_CACHED = 14,
        TRAP_EXPENSIVE = 6,
        TRAP_MACRO = 15,
        TRAP_UNASSIGNED = 0,
        TRAP_UNBOUND = 2
    }


    [Serializable]
    sealed class ReferenceTrap : ISerializable
    {
        public static TrapKind GetTrapKind (object value)
        {
            ReferenceTrap reftrap = value as ReferenceTrap;
            if (reftrap == null)
                return TrapKind.NON_TRAP_KIND;
            else if (reftrap == expensive)
                return TrapKind.TRAP_EXPENSIVE;
            else if (reftrap == unassigned)
                return TrapKind.TRAP_UNASSIGNED;
            else if (reftrap == unbound)
                return TrapKind.TRAP_UNBOUND;
            else
                return (TrapKind) (reftrap.contents.Car);
        }
        
        static ReferenceTrap expensive;
        static ReferenceTrap unassigned;
        static ReferenceTrap unbound;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        Cons contents;

        ReferenceTrap ()
        {
            contents = null;
        }

        public ReferenceTrap (Cons obj)
        {
            this.contents = obj;
        }

        public object Contents
        {
            [DebuggerStepThrough]
            get
            {
                return this.contents;
            }
        }

        public static ReferenceTrap Expensive
        {
            get
            {
                if (expensive == null)
                    expensive = new ReferenceTrap ();
                return expensive;
            }
        }

        public static ReferenceTrap Unassigned
        {
            get
            {
                if (unassigned == null)
                    unassigned = new ReferenceTrap ();
                return unassigned;
            }
        }

        public static ReferenceTrap Unbound
        {
            get
            {
                if (unbound == null)
                    unbound = new ReferenceTrap ();
                return unbound;
            }
        }

        public static ReferenceTrap Make (object arg)
        {
            if (arg is int)
                return Make ((int) arg);
            else if (arg is Cons)
                return Make ((Cons) arg);
            else
                throw new NotImplementedException ();
        }

        public static ReferenceTrap Make (int arg)
        {
            if (arg == 0)
                return Unassigned;
            else if (arg == 2)
                return Unbound;
            else
                throw new NotImplementedException ();
        }

        public static ReferenceTrap Make (Cons arg)
        {
            return new ReferenceTrap (arg);
        }

        [SchemePrimitive ("REFERENCE-TRAP?", 1, true)]
        public static bool IsReferenceTrap (out object answer, object arg)
        {
            answer = arg is ReferenceTrap;
            return false;
        }


        #region ISerializable Members

        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        public void GetObjectData (SerializationInfo info, StreamingContext context)
        {
            switch (GetTrapKind (this)) {
                case TrapKind.TRAP_UNASSIGNED:
                    info.SetType (typeof (UnassignedReferenceTrapDeserializer));
                    return;
                default:
                    throw new NotImplementedException ();
            }
        }
        #endregion
    }

    [Serializable]
    sealed class UnassignedReferenceTrapDeserializer : ISerializable, IObjectReference
    {

        UnassignedReferenceTrapDeserializer (SerializationInfo info, StreamingContext context)
        {
        }

        #region ISerializable Members

        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        void ISerializable.GetObjectData (SerializationInfo info, StreamingContext context)
        {
            throw new NotImplementedException ();
        }
        #endregion

        #region IObjectReference Members

        public object GetRealObject (StreamingContext context)
        {
            return ReferenceTrap.Unassigned;
        }
        #endregion

    }
}
