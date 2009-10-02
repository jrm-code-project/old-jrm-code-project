using System;
using System.Collections;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Text;
using System.Runtime.Serialization;
using System.Runtime.Serialization.Formatters.Binary;

namespace Microcode
{
    enum FaslArch
    {
        FASL_UNKNOWN,
        FASL_PDP10,
        FASL_VAX,
        FASL_68020,
        FASL_68000,
        FASL_HP_9000_500,
        FASL_IA32,
        FASL_BFLY,
        FASL_CYBER,
        FASL_CELERITY,
        FASL_HP_SPECTRUM,
        FASL_UMAX,
        FASL_PYR,
        FASL_ALLIANT,
        FASL_SPARC,
        FASL_MIPS,
        FASL_APOLLO_68K,
        FASL_APOLLO_PRISM,
        FASL_ALPHA,
        FASL_RS6000,
        FASL_PPC32,
        FASL_X86_64,
        FASL_PPC64,
        FASL_IA64
    }

    enum FaslVersion
    {
        FASL_VERSION_NONE,
        FASL_VERSION_LONG_HEADER = 3,
        FASL_VERSION_DENSE_TYPES,
        FASL_VERSION_PADDED_STRINGS,
        FASL_VERSION_REFERENCE_TRAP,
        FASL_VERSION_MERGED_PRIMITIVES,
        FASL_VERSION_INTERFACE_VERSION,
        FASL_VERSION_NEW_BIGNUMS,
        FASL_VERSION_C_CODE,
        FASL_VERSION_STACK_END
    }

    class FaslSection
    {
        public int sectionBase;
        int sectionLimit;
        EncodedObject [] contents;

        FaslSection (int sectionBase, int sectionLimit, EncodedObject [] contents)
        {
            this.sectionBase = sectionBase;
            this.sectionLimit = sectionLimit;
            this.contents = contents;
        }

        public bool Contains (uint offset)
        {
            return (offset >= this.sectionBase) && (offset < this.sectionLimit);
        }

        internal EncodedObject this [uint offset]
        {
            get
            {
                return this.contents [(offset - this.sectionBase) / 4];
            }
        }

        public byte ReadByte (uint offset)
        {
            return this [offset].GetByte ((byte) (offset % 4));
        }

        public object ReadBignum (FaslFile file, uint offset)
        {
            //EncodedObject header = this [offset];
            EncodedObject h1 = this [offset + 4];
            if (h1.Datum == 1)
            {
                EncodedObject w0 = this [offset + 8];
                long total = w0.ToLong ();
                return total;
            }
            if (h1.Datum == 2)
            {
                EncodedObject w0 = this [offset + 8];
                EncodedObject w1 = this [offset + 12];
                long total = (w1.ToLong () << 30) + w0.ToLong ();
                return total;
            }
            throw new NotImplementedException ();
        }

        public double ReadFlonum (FaslFile file, uint offset)
        {
            //EncodedObject header = this [offset];
            byte b0 = ReadByte (offset + 4);
            byte b1 = ReadByte (offset + 5);
            byte b2 = ReadByte (offset + 6);
            byte b3 = ReadByte (offset + 7);
            byte b4 = ReadByte (offset + 8);
            byte b5 = ReadByte (offset + 9);
            byte b6 = ReadByte (offset + 10);
            byte b7 = ReadByte (offset + 11);
            if ((b0 == 0) &&
                (b1 == 0) &&
                (b2 == 0) &&
                (b3 == 0) &&
                (b4 == 0) &&
                (b5 == 0) &&
                (b6 == 0) &&
                (b7 == 0))
                return 0.0;
            int encodedExponent = (b7 & 0x7F) << 4;
            encodedExponent += (b6 & 0xF0) >> 4;
            long mantissa = (encodedExponent == 0) ? 0 : 1;
            mantissa *= 16;
            mantissa += b6 & 0x0F;
            mantissa *= 256;
            mantissa += b5;
            mantissa *= 256;
            mantissa += b4;
            mantissa *= 256;
            mantissa += b3;
            mantissa *= 256;
            mantissa += b2;
            mantissa *= 256;
            mantissa += b1;
            mantissa *= 256;
            mantissa += b0;
            int exponent = -(1024 + 51);
            exponent += encodedExponent;
            int sign = ((b7 & 0x80) == 0) ? 1 : -1;
            double answer = FloatArithmetic.EncodeFloat (sign, exponent, mantissa);
            return answer;
        }


        public char [] ReadString (uint offset)
        {
            // EncodedObject header = this [offset];
            uint length = this [offset + 4].Datum;
            char [] result = new char [length];
            for (uint i = 0; i < length; i++)
                result [i] = (char) (this.ReadByte (offset + 8 + i));
            return result;
        }

        public string [] ReadFormals (FaslFile file, uint offset)
        {
            uint length = this [offset].Datum;
            string [] vector = new string [length];
            for (uint i = 0; i < length; i++)
                vector [i] = (string) (file.ReadObject (offset + 4 + i * 4));
            return vector;
        }

        public object [] ReadVector (FaslFile file, uint offset)
        {
            uint length = this [offset].Datum;
            object [] vector = new object [length];
            for (uint i = 0; i < length; i++)
                vector [i] = file.ReadObject (offset + 4 + i * 4);
            return vector;
        }

        public static FaslSection Load (BinaryReader binaryReader, int sectionBase, int sectionLimit, UInt32 count)
        {
            EncodedObject [] contents = new EncodedObject [count];
            for (int i = 0; i < count; i++)
                contents [i] = new EncodedObject (binaryReader.ReadUInt32 ());
            return new FaslSection (sectionBase, sectionBase + (sectionLimit * 4), contents);
        }
    }

    [Serializable]
    class BadFaslFileException : Exception
    {
        public BadFaslFileException ()
             { }
        public BadFaslFileException (String message)
            : base (message) { }
        public BadFaslFileException (String message, Exception innerException)
            : base (message, innerException) { }
    }

    class FaslHeader
    {
        const UInt32 FASL_FILE_MARKER = 0xfafafafa;
        const int FASL_HEADER_SIZE = 50;

        public UInt32 heapCount;
        public UInt32 heapBase;
        public UInt32 dumpedObj;
        public UInt32 constCount;
        public UInt32 constBase;
        UInt32 version;
        UInt32 stackTop;
        public UInt32 primLength;
        public UInt32 primSize;
        UInt32 ciVersion;
        UInt32 utBase;
        UInt32 checkSum;
        UInt32 cLength;
        public UInt32 cSize;
        UInt32 memBase;

        public FaslHeader (UInt32 heapCount,
                           UInt32 heapBase,
                           UInt32 dumpedObj,
                           UInt32 constCount,
                           UInt32 constBase,
                           UInt32 version,
                           UInt32 stackTop,
                           UInt32 primLength,
                           UInt32 primSize,
                           UInt32 ciVersion,
                           UInt32 utBase,
                           UInt32 checkSum,
                           UInt32 cLength,
                           UInt32 cSize,
                           UInt32 memBase)
        {
            this.heapCount = heapCount;
            this.heapBase = heapBase;
            this.dumpedObj = dumpedObj;
            this.constCount = constCount;
            this.constBase = constBase;
            this.version = version;
            this.stackTop = stackTop;
            this.primLength = primLength;
            this.primSize = primSize;
            this.ciVersion = ciVersion;
            this.utBase = utBase;
            this.checkSum = checkSum;
            this.cLength = cLength;
            this.cSize = cSize;
            this.memBase = memBase;
        }

        const UInt32 DATUM_MASK = 0x03FFFFFF;

        public static FaslHeader Load (BinaryReader binaryReader)
        {
            UInt32 magic = binaryReader.ReadUInt32 ();
            if (magic != FASL_FILE_MARKER)
                throw new NotImplementedException ();
            UInt32 heapCount = binaryReader.ReadUInt32 () & DATUM_MASK;
            UInt32 heapBase = binaryReader.ReadUInt32 () & DATUM_MASK;
            UInt32 dumpedObj = binaryReader.ReadUInt32 () & DATUM_MASK; /* Where dumped object was */
            UInt32 constCount = binaryReader.ReadUInt32 () & DATUM_MASK;        /* Count of objects in const. area */
            UInt32 constBase = binaryReader.ReadUInt32 () & DATUM_MASK; /* Address of const. area at dump */
            UInt32 version = binaryReader.ReadUInt32 ();        /* FASL format version info. */
            UInt32 stackTop = binaryReader.ReadUInt32 ();       /* Top of stack when dumped */
            UInt32 primLength = binaryReader.ReadUInt32 () & DATUM_MASK;     /* Number of entries in primitive primitiveTable */
            UInt32 primSize = binaryReader.ReadUInt32 () & DATUM_MASK;  /* Size of primitive primitiveTable in SCHEME_OBJECTs */
            UInt32 ciVersion = binaryReader.ReadUInt32 ();      /* Version number for compiled code interface */
            UInt32 utBase = binaryReader.ReadUInt32 (); /* Address of the utilities vector */
            UInt32 checkSum = binaryReader.ReadUInt32 ();               /* Header and data checksum. */
            UInt32 cLength = binaryReader.ReadUInt32 ();        /* Number of entries in the C code primitiveTable */
            UInt32 cSize = binaryReader.ReadUInt32 () & DATUM_MASK;     /* Size of C code primitiveTable in SCHEME_OBJECTs */
            UInt32 memBase = binaryReader.ReadUInt32 ();
            FaslHeader header = new FaslHeader (
                                                heapCount,
                                                heapBase,
                                                dumpedObj,
                                                constCount,
                                                constBase,
                                                version,
                                                stackTop,
                                                primLength,
                                                primSize,
                                                ciVersion,
                                                utBase,
                                                checkSum,
                                                cLength,
                                                cSize,
                                                memBase);
            for (int i = 16; i < FASL_HEADER_SIZE; i++)
            {
                UInt32 discard = binaryReader.ReadUInt32 ();
                if (discard != 0)
                    throw new BadFaslFileException ();
            }
            return header;
        }
    }

    class FaslFile
    {
        static readonly object sharpT = true;
        static readonly object sharpF = false;
        FaslHeader faslHeader;
        FaslSection heapSection;
        FaslSection constSection;
        Primitive [] primSection;
        FaslSection cSection;
        Dictionary<uint, object> sharingTable;

        public FaslFile (FaslHeader faslHeader, FaslSection heapSection, FaslSection constSection, Primitive [] primSection, FaslSection cSection)
        {
            this.faslHeader = faslHeader;
            this.heapSection = heapSection;
            this.constSection = constSection;
            this.primSection = primSection;
            this.cSection = cSection;
            this.sharingTable = new Dictionary<uint, object> ();
        }

        internal static FaslFile Fasload (BinaryReader binaryReader)
        {
            FaslHeader faslHeader = FaslHeader.Load (binaryReader);
            FaslSection heapSection = FaslSection.Load (binaryReader, Strip (faslHeader.heapBase), Strip (faslHeader.heapCount), faslHeader.heapCount);
            FaslSection constSection = FaslSection.Load (binaryReader, Strip (faslHeader.constBase), Strip (faslHeader.constCount), faslHeader.constCount);
            Primitive [] primTable = LoadPrimitives (binaryReader, 0, (uint) Strip (faslHeader.primSize), faslHeader.primLength);
            FaslSection cTable = FaslSection.Load (binaryReader, 0, Strip (faslHeader.cSize), faslHeader.cSize);

            return new FaslFile (faslHeader, heapSection, constSection, primTable, cTable);
        }

        public static Primitive [] LoadPrimitives (BinaryReader binaryReader, uint sectionBase, UInt32 count, UInt32 nPrims)
        {
            EncodedObject [] sectionContents = new EncodedObject [count];
            for (int i = 0; i < count; i++)
                sectionContents [i] = new EncodedObject (binaryReader.ReadUInt32 ());

            Primitive [] primitives = new Primitive [nPrims];
            uint offset = sectionBase;
            for (uint i = 0; i < primitives.Length; i++)
            {
                EncodedObject arity = sectionContents [offset / 4];
                offset += 4;
                uint stringLength = sectionContents [(offset / 4) + 1].Datum;
                char [] result = new char [stringLength];
                for (uint j = 0; j < stringLength; j++)
                {
                    EncodedObject encoded = sectionContents [(offset + 8 + j) / 4];
                    result [j] = (char) encoded.GetByte ((byte) ((offset + 8 + j) % 4));
                }
                string name = new string (result);

                offset += (sectionContents [offset / 4].Datum) * 4;
                offset += 4;
                primitives [i] = Primitive.Find (name, arity.Datum == 0x03ffffff ? -1 : (int) arity.Datum);
            }
            return primitives;
        }

        public object ReadBignum (uint location)
        {
            return heapSection.Contains (location) ? heapSection.ReadBignum (this, location)
                : constSection.Contains (location) ? constSection.ReadBignum (this, location)
                : 0;
        }

        public double ReadBigFlonum (uint location)
        {
            return heapSection.Contains (location) ? heapSection.ReadFlonum (this, location)
                : constSection.Contains (location) ? constSection.ReadFlonum (this, location)
                : 0.0;
        }

        public object [] ReadVector (uint location)
        {
            return heapSection.Contains (location) ? heapSection.ReadVector (this, location)
                : constSection.Contains (location) ? constSection.ReadVector (this, location)
                : null;
        }

        public Symbol [] ReadFormals (uint location)
        {
            object [] formals = (object []) ReadObject (location);
            Symbol [] result = new Symbol [formals.Length];
            for (int i = 0; i < formals.Length; i++)
                result [i] = (Symbol) formals [i];
            return result;
        }

        public void ReadFormals (uint location, out Symbol name, out Symbol [] formals)
        {
            object [] names = (object []) ReadObject (location);
            for (int i = 0; i < names.Length - 1; i++)
                for (int j = i + 1; j < names.Length; j++)
                    if (names [i] == names [j])
                        Debugger.Break ();


            name = (Symbol) names [0];
            formals = new Symbol [names.Length-1];
            for (int i = 1; i < names.Length; i++)
                formals [i - 1] = (names [i] is Symbol) ? (Symbol) names [i] : Symbol.MakeUninterned ((string) names [i]);
            return;
        }

        public SCode ReadAssignment (uint location)
        {
            return Assignment.Make (((Variable) ReadObject (location)),
                                   ReadObject (location + 4));
        }

        public SCode ReadExtendedLambda (uint location)
        {
            Symbol name;
            Symbol [] formals;
            ReadFormals (location + 4, out name, out formals);
            object third = ReadObject (location + 8);
            EncodedObject argcount = new EncodedObject ((uint) (int) third);
            uint optional = (argcount.Datum & 0x00FF);
            uint required = (argcount.Datum & 0xFF00) >> 8;
            bool rest = ((argcount.Datum & 0x10000) == 0x10000);
            SCode body = SCode.EnsureSCode (ReadObject (location));
            return ExtendedLambda.Make (name, formals, body, required, optional, rest);
        }

        static int gensymCounter;

        internal object ReadObject (uint location)
        {
            object probe = null;
            if (this.sharingTable.TryGetValue (location, out probe) == true)
                return probe;

            EncodedObject encoded = 
                  heapSection.Contains (location) ? heapSection [location]
                : constSection.Contains (location) ? constSection [location]
                : new EncodedObject (0);
            // Console.WriteLine ("{0}", encoded.TypeCode);
            object first = null;
            switch (encoded.TypeCode)
            {
                case TC.ACCESS:
                    return  Access.Make (ReadObject (encoded.Datum),
                       (Symbol) ReadObject (encoded.Datum + 4));

                case TC.ASSIGNMENT:
                    return ReadAssignment (encoded.Datum);

                case TC.BIG_FIXNUM:
                    return ReadBignum (encoded.Datum);

                case TC.BIG_FLONUM:
                    return ReadBigFlonum (encoded.Datum);

                case TC.CHARACTER:
                    return (char) (encoded.Datum);

                case TC.CHARACTER_STRING:
                    return heapSection.ReadString (encoded.Datum);

                case TC.COMBINATION:
                    return Combination.Make (ReadVector (encoded.Datum));

                case TC.COMBINATION_1:
                    return Combination1.Make (ReadObject (encoded.Datum),
                                             ReadObject (encoded.Datum + 4));

                case TC.COMBINATION_2:
                    return Combination2.Make (ReadObject (encoded.Datum),
                                              ReadObject (encoded.Datum + 4),
                                              ReadObject (encoded.Datum + 8));

                case TC.COMMENT:
                    return Comment.Make (ReadObject (encoded.Datum),
                                         ReadObject (encoded.Datum + 4));

                case TC.COMPLEX:
                    return new Complex (ReadObject (encoded.Datum),
                                        ReadObject (encoded.Datum + 4));

                case TC.CONDITIONAL:
                    return Conditional.Make (ReadObject (encoded.Datum),
                                             ReadObject (encoded.Datum + 4),
                                             ReadObject (encoded.Datum + 8));

                case TC.CONSTANT:
                    return Constant.Decode (encoded.Datum);
 
                case TC.DEFINITION:
                    return Definition.Make ((Symbol) ReadObject (encoded.Datum),
                                           ReadObject (encoded.Datum + 4));

                case TC.DELAY:
                    return Delay.Make (ReadObject (encoded.Datum));

                case TC.DISJUNCTION:
                    return Disjunction.Make (ReadObject (encoded.Datum),
                                            ReadObject (encoded.Datum + 4));

                case TC.EXTENDED_LAMBDA:
                    return ReadExtendedLambda (encoded.Datum);

                case TC.FIXNUM:
                    return encoded.Datum > 0x02000000
                           ? (int) - (0x04000000 - encoded.Datum)
                           : (int) encoded.Datum;

                case TC.INTERNED_SYMBOL:
                    return Symbol.Make (new String ((char []) ReadObject (encoded.Datum)));

                case TC.LAMBDA:
                    Symbol name;
                    Symbol [] formals;
                    ReadFormals (encoded.Datum + 4, out name, out formals);
                    return Lambda.Make (name, formals, SCode.EnsureSCode (ReadObject (encoded.Datum)));

                case TC.LIST:
                    object second = ReadObject (encoded.Datum + 4);
                    return new Cons (ReadObject (encoded.Datum),
                                     second == sharpF ? null : second);

                case TC.NULL:
                    if (encoded.Datum != 0)
                        throw new NotImplementedException ();
                    return sharpF;

                 case TC.PCOMB0:
                    return PrimitiveCombination0.Make ((Primitive0) primSection[encoded.Datum]);

                case TC.PCOMB1:
                    return PrimitiveCombination1.Make ((Primitive1) ReadObject (encoded.Datum),
                                                       ReadObject (encoded.Datum + 4));

                case TC.PCOMB2:
                    return PrimitiveCombination2.Make ((Primitive2) ReadObject (encoded.Datum),
                                                        ReadObject (encoded.Datum + 4),
                                                        ReadObject (encoded.Datum + 8));

                case TC.PCOMB3:
                    return PrimitiveCombination3.Make ((Primitive3) ReadObject (encoded.Datum + 4),
                                                       ReadObject (encoded.Datum + 8),
                                                       ReadObject (encoded.Datum + 12),
                                                       ReadObject (encoded.Datum + 16));

                case TC.PRIMITIVE:
                    return primSection [encoded.Datum];

                case TC.REFERENCE_TRAP:
                    if (encoded.Datum == 0)
                        return ReferenceTrap.Unassigned;
                    else
                        throw new NotImplementedException ();
                    // return ReferenceTrap.Make (encoded.Datum);

                case TC.RATNUM:
                    return new Ratnum (ReadObject (encoded.Datum),
                                       ReadObject (encoded.Datum + 4));

                case TC.RETURN_CODE:
                    return (ReturnCode) (encoded.Datum);

                case TC.SEQUENCE_2:
                    return Sequence2.Make (ReadObject (encoded.Datum),
                                          ReadObject (encoded.Datum + 4));

                case TC.SEQUENCE_3:
                    // Chains of sequence_3 can be arbitrarily long.
                    // Unfortunately, the CLR puts a strict limit on
                    // the stack, so we have to do this funky thing.
                    Cons sequenceStack = null;
                    while (true) {
                        // read the first two elements
                        object s1 = ReadObject (encoded.Datum);
                        sequenceStack = new Cons (s1, sequenceStack);
                        object s2 = ReadObject (encoded.Datum + 4);
                        sequenceStack = new Cons (s2, sequenceStack);

                        // peek at the third

                        EncodedObject sencoded = 
                             heapSection.Contains (encoded.Datum + 8) ? heapSection [encoded.Datum + 8]
                           : constSection.Contains (encoded.Datum + 8) ? constSection [encoded.Datum + 8]
                           : new EncodedObject (0);

                        if (sencoded.TypeCode == TC.SEQUENCE_3)
                            encoded = sencoded;
                        else {
                            // found the end of the chain!
                            object tail = ReadObject (encoded.Datum + 8);
                            while (sequenceStack != null) {
                                object ob2 = sequenceStack.Car;
                                sequenceStack = (Cons) sequenceStack.Cdr;
                                object ob1 = sequenceStack.Car;
                                sequenceStack = (Cons) sequenceStack.Cdr;
                                tail = Sequence3.Make (ob1, ob2, tail);
                            }
                            return tail;
                        }
                    }


                case TC.THE_ENVIRONMENT:
                    return TheEnvironment.Make ();

                case TC.UNINTERNED_SYMBOL:
                    // KLUDGE!!  Make sure that all uninterned strings within a file
                    // keep their identity when read.
                    // Also, postpend a unique number so we can tell these apart.
                    first = ReadObject (encoded.Datum);
                    if (first is Symbol)
                        return first;
                    else
                    {
                        Symbol result = Symbol.MakeUninterned ("#:" + new String ((char []) first) + "-" + (gensymCounter++).ToString(CultureInfo.InvariantCulture));
                        this.sharingTable.Add (encoded.Datum, result);
                        return result;
                    }

                case TC.VARIABLE:
                    return Variable.Make ((Symbol) ReadObject (encoded.Datum));

                case TC.VECTOR:
                    return ReadVector (encoded.Datum);

                default:
                    throw new NotImplementedException ();
            }
        }

        public object RootObject
        {
            get
            {
                return this.ReadObject (faslHeader.dumpedObj & 0x03FFFFFFF);
            }
        }

        static int Strip (UInt32 encoded)
        {
            return (int) (encoded & 0x03FFFFFFF);
        }
    }

    public sealed class Fasl
    {
        private Fasl () { }

        static object OldFasload (string pathName)
        {
            FileStream faslStream = null;

            try {
                faslStream = File.OpenRead (pathName);
                FaslFile faslFile = FaslFile.Fasload (new BinaryReader (faslStream));
                return faslFile.RootObject;
            }
            finally
            {
                if (faslStream != null)
                    faslStream.Close ();
            }
        }

        static object NewFasload (string pathName)
        {
            //if (pathName == "c:\\jrm-code-project\\mit-scheme\\cref\\anfile.bin")
            //    Debugger.Break ();
            FileStream faslStream = null;

            try {
                faslStream = File.OpenRead (pathName);
                faslStream.ReadByte ();
                faslStream.ReadByte ();
                faslStream.ReadByte ();
                faslStream.ReadByte ();
                BinaryFormatter bfmt = new BinaryFormatter ();
                object rootObject = bfmt.Deserialize (faslStream);
                return rootObject;
            }
            finally {
                if (faslStream != null)
                    faslStream.Close ();
            }
        }

        public static bool EnableOldFasload;

        public static object Fasload (string pathName)
        {
            if (EnableOldFasload) {
                try {
                    return NewFasload (pathName);
                }
                catch (SerializationException) {
                    return OldFasload (pathName);
                }
            }
            else {
                return NewFasload (pathName);
            }
        }

        [SchemePrimitive ("BINARY-FASLOAD", 1, false)]
        public static bool BinaryFasload (out object answer, object arg)
        {
            answer = Fasload (new String ((char []) arg));
            return false;
        }
    }
}
