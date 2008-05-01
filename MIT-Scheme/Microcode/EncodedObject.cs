using System;
using System.Diagnostics;
using System.Globalization;

// Objects in a legacy fasl file use the legacy encoding
// which is described here.
namespace Microcode
{
    struct EncodedObject
    {
        readonly UInt32 representation;

        public const int OBJECT_LENGTH = 32;
        public const int TYPE_CODE_LENGTH = 6;
        public const int HALF_DATUM_LENGTH = 13;
        public const int HALF_DATUM_MASK = 0x00001FFF;
        public const int DATUM_LENGTH = 26;
        public const int DATUM_MASK = 0x03FFFFFF;

        public EncodedObject (UInt32 encodedValue)
        {
            this.representation = encodedValue;
        }

        [DebuggerStepThrough]
        public static explicit operator uint (EncodedObject e)
        {
            return e.representation;
        }

        [DebuggerStepThrough]
        public long ToLong ()
        {
            return (long) (this.representation);
        }

        public byte GetByte (byte which)
        {
            switch (which)
            {
                case 0:
                    return (byte) (representation & 0x000000FF);
                case 1:
                    return (byte) ((representation & 0x0000FF00) >> 8);
                case 2:
                    return (byte) ((representation & 0x00FF0000) >> 16);
                case 3:
                    return (byte) ((representation & 0xFF000000) >> 24);
                default:
                    throw new NotImplementedException ();
            }
        }

        public override string ToString ()
        {
            return "##<" + this.TypeCode.ToString () + " " + this.Datum.ToString ("x",CultureInfo.CurrentCulture) + ">";
        }

        public TC TypeCode
        {
            [DebuggerStepThrough]
            get
            {
                return (TC) (this.representation >> DATUM_LENGTH);
            }
        }

        public uint Datum
        {
            [DebuggerStepThrough]
            get
            {
                return (uint) (this.representation & DATUM_MASK);
            }
        }
    }
}
