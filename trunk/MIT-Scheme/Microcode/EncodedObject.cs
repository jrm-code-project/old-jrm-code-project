using System;
using System.Globalization;

// Objects in a legacy fasl file use the legacy encoding
// which is described here.
namespace Microcode
{
    enum TC
    {
        NULL = 0x00,
        LIST = 0x01,
        CHARACTER = 0x02,
        SCODE_QUOTE = 0x03,
        PCOMB2 = 0x04,
        UNINTERNED_SYMBOL = 0x05,
        BIG_FLONUM = 0x06,
        COMBINATION_1 = 0x07,
        CONSTANT = 0x08,
        EXTENDED_PROCEDURE = 0x09,
        VECTOR = 0x0A,
        RETURN_CODE = 0x0B,
        COMBINATION_2 = 0x0C,
        MANIFEST_CLOSURE = 0x0D,
        BIG_FIXNUM = 0x0E,
        PROCEDURE = 0x0F,
        ENTITY = 0x10 /* PRIMITIVE_EXTERNAL */,
        DELAY = 0x11,
        ENVIRONMENT = 0x12,
        DELAYED = 0x13,
        EXTENDED_LAMBDA = 0x14,
        COMMENT = 0x15,
        NON_MARKED_VECTOR = 0x16,
        LAMBDA = 0x17,
        PRIMITIVE = 0x18,
        SEQUENCE_2 = 0x19,
        FIXNUM = 0x1A,
        PCOMB1 = 0x1B,
        CONTROL_POINT = 0x1C,
        INTERNED_SYMBOL = 0x1D,
        CHARACTER_STRING = 0x1E,
        ACCESS = 0x1F,
        HUNK3_A = 0x20 /* EXTENDED_FIXNUM */,
        DEFINITION = 0x21,
        BROKEN_HEART = 0x22,
        ASSIGNMENT = 0x23,
        HUNK3_B = 0x24,
        IN_PACKAGE = 0x25,
        COMBINATION = 0x26,
        MANIFEST_NM_VECTOR = 0x27,
        COMPILED_ENTRY = 0x28,
        LEXPR = 0x29,
        PCOMB3 = 0x2A,
        MANIFEST_SPECIAL_NM_VECTOR = 0x2B,
        VARIABLE = 0x2C,
        THE_ENVIRONMENT = 0x2D,
        FUTURE = 0x2E,
        VECTOR_1B = 0x2F,
        PCOMB0 = 0x30,
        VECTOR_16B = 0x31,
        REFERENCE_TRAP = 0x32 /* UNASSIGNED */,
        SEQUENCE_3 = 0x33,
        CONDITIONAL = 0x34,
        DISJUNCTION = 0x35,
        CELL = 0x36,
        WEAK_CONS = 0x37,
        QUAD = 0x38,
        LINKAGE_SECTION = 0x39,
        RATNUM = 0x3A /* COMPILER_LINK */,
        STACK_ENVIRONMENT = 0x3B,
        COMPLEX = 0x3C,
        COMPILED_CODE_BLOCK = 0x3D,
        RECORD = 0x3E
    }

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

        public static explicit operator uint (EncodedObject e)
        {
            return e.representation;
        }

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
            get
            {
                return (TC) (this.representation >> DATUM_LENGTH);
            }
        }

        public uint Datum
        {
            get
            {
                return (uint) (this.representation & DATUM_MASK);
            }
        }
    }
}
