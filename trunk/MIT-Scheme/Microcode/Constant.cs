using System;
using System.Diagnostics;
using System.Runtime.Serialization;
using System.Security.Permissions;

namespace Microcode
{
    [Serializable]
    public sealed class Constant : SchemeObject, ISerializable
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public override TC TypeCode { get { return TC.CONSTANT; } }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public static object sharpT = true;
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public static Constant theDefaultObject = new Constant ("default");
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public static Constant theEofObject = new Constant ("eof");
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public static Constant theAuxMarker = new Constant ("theAuxMarker");
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public static Constant theKeyMarker = new Constant ("theKeyMarker");
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public static Constant theOptionalMarker = new Constant ("theOptionalMarker");
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public static Constant theRestMarker = new Constant ("theRestMarker");
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public static Constant theExternalUnassignedObject = new Constant ("Unassigned");
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public static Constant theUnspecificObject = new Constant ("Unspecific");
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public static object sharpF = false;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        string name;

        private Constant (string name)
        {
            this.name = name;
        }

        public override string ToString ()
        {
            return "#!" + this.name;
        }

        internal static object Decode (uint code)
        {
            switch (code)
            {
                case 0:
                    return sharpT;
                case 1:
                    return Unspecific;
                case 2:
                    return ExternalUnassigned;
                case 3:
                    return LambdaOptionalTag;
                case 4:
                    return LambdaRestTag;
                case 5:
                    return LambdaKeyTag;
                case 6:
                    return EofObject;
                case 7:
                    return DefaultObject;
                case 8:
                    return LambdaAuxTag;
                case 9:
                    return null;
                default:
                    throw new NotImplementedException ();
            }
        }

        public static Constant DefaultObject
        {
            [DebuggerStepThrough]
            get
            {
                return theDefaultObject;
            }
        }

        public static Constant EofObject
        {
            [DebuggerStepThrough]
            get
            {
                return theEofObject;
            }
        }

        public static Constant LambdaAuxTag
        {
            [DebuggerStepThrough]
            get
            {
                return theAuxMarker;
            }
        }

        public static Constant LambdaKeyTag
        {
            [DebuggerStepThrough]
            get
            {
                return theKeyMarker;
            }
        }


        public static Constant LambdaOptionalTag
        {
            [DebuggerStepThrough]
            get
            {
                return theOptionalMarker;
            }
        }

        public static Constant LambdaRestTag
        {
            [DebuggerStepThrough]
            get
            {
                return theRestMarker;
            }
        }

        public static Constant ExternalUnassigned
        {
            [DebuggerStepThrough]
            get
            {
                return theExternalUnassignedObject;
            }
        }

        public static Constant Unspecific
        {
            [DebuggerStepThrough]
            get
            {
                return theUnspecificObject;
            }
        }

        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        void ISerializable.GetObjectData (SerializationInfo info, StreamingContext context)
        {
            info.SetType (typeof (ConstantDeserializer));
            info.AddValue ("name", this.name);
        }
    }

    [Serializable]
    internal sealed class ConstantDeserializer : IObjectReference
    {
        String name;

        Object BadConstant ()
        {
            throw new NotImplementedException ();
        }

        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        public Object GetRealObject (StreamingContext context)
        {
            return
                this.name == "default" ? Constant.DefaultObject :
                this.name == "eof" ? Constant.EofObject :
                this.name == "theAuxMarker" ? Constant.LambdaAuxTag :
                this.name == "theKeyMarker" ? Constant.LambdaKeyTag :
                this.name == "theOptionalMarker" ? Constant.LambdaOptionalTag :
                this.name == "theRestMarker" ? Constant.LambdaRestTag :
                this.name == "Unassigned" ? Constant.ExternalUnassigned :
                this.name == "Unspecific" ? Constant.Unspecific :
                BadConstant ();
        }

        // shut up compiler
        public void SetName (String name) { this.name = name; }
    }
}
