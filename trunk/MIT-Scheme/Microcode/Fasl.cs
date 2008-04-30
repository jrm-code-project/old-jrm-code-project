using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Text;


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
            EncodedObject encoded = this.contents [(offset - this.sectionBase) / 4];
            return encoded.GetByte ((byte) (offset % 4));
        }

        //public object ReadBignum (FaslFile file, int offset)
        //{
        //    EncodedObject header = this  [offset];
        //    EncodedObject h1 = this [offset + 4];
        //    if (h1.Datum == 1)
        //    {
        //        EncodedObject w0 = this [offset + 8);
        //        long total = w0.ToLong ();
        //        return total;
        //    }
        //    if (h1.Datum == 2)
        //    {
        //        EncodedObject w0 = this [offset + 8];
        //        EncodedObject w1 = this [offset + 12];
        //        long total = (w1.ToLong () << 30) + w0.ToLong ();
        //        return total;
        //    }
        //    throw new NotImplementedException ();
        //}

        public char [] ReadString (uint offset)
        {
            EncodedObject header = this [offset];
            uint length = this [offset + 4].Datum;
            char [] result = new char [length];
            for (uint i = 0; i < length; i++)
                result [i] = (char) (this.ReadByte (offset + 8 + i));
            return result;
        }

        internal SCode [] ReadSCodeVector (FaslFile file, uint offset)
        {
            uint length = this [offset].Datum;
            SCode [] vector = new SCode [length];
            for (uint i = 0; i < length; i++)
                vector [i] = file.ReadSCode (offset + 4 + i * 4);
            return vector;
        }

        public string [] ReadFormals (FaslFile file, uint offset)
        {
            uint length = this [offset].Datum;
            string [] vector = new string [length];
            for (uint i = 0; i < length; i++)
                vector [i] = (string) (file.ReadObject (offset + 4 + i * 4));
            return vector;
        }

        public object ReadVector (FaslFile file, uint offset)
        {
            EncodedObject header = this [offset];
            uint length = header.Datum;
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

    class FaslHeader
    {
        const UInt32 FASL_FILE_MARKER = 0xfafafafa;

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
            for (int i = 16; i < 50; i++)
            {
                UInt32 discard = binaryReader.ReadUInt32 ();
                Debug.Assert (discard == 0);
            }
            return header;
        }
    }

    class FaslFile
    {
        static readonly object sharpT = true;
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
                Debug.WriteLine ("Primitive " + name);
                primitives [i] = Primitive.Find (name, arity.Datum == 0x03ffffff ? -1 : (int) arity.Datum);
            }
            return primitives;
        }

        //public object ReadBignum (int location)
        //{
        //    return heapSection.Contains (location) ? heapSection.ReadBignum (this, location)
        //        : constSection.Contains (location) ? constSection.ReadBignum (this, location)
        //        : 0;
        //}

        //public double ReadBigFlonum (int location)
        //{
        //    return heapSection.Contains (location) ? heapSection.ReadFlonum (this, location)
        //        : constSection.Contains (location) ? constSection.ReadFlonum (this, location)
        //        : 0.0;
        //}

        public object ReadVector (uint location)
        {
            return heapSection.Contains (location) ? heapSection.ReadVector (this, location)
                : constSection.Contains (location) ? constSection.ReadVector (this, location)
                : null;
        }

        public SCode [] ReadSCodeVector (uint location)
        {
            return heapSection.Contains (location) ? heapSection.ReadSCodeVector (this, location)
                   : constSection.Contains (location) ? constSection.ReadSCodeVector (this, location)
                   : null;
        }

        public string [] ReadFormals (uint location)
        {
            object [] formals = (object []) ReadObject (location);
            string [] result = new string [formals.Length];
            for (int i = 0; i < formals.Length; i++)
                result [i] = (string) formals [i];
            return result;
        }

        //public Access ReadAccess (int location)
        //{
        //    return new Access (SCode.Convert (ReadObject (location)),
        //                       (string) ReadObject (location + 4));
        //}

        public Assignment ReadAssignment (uint location)
        {
            return new Assignment (((Variable) ReadObject (location)).name,
                                   ReadSCode (location + 4));
        }

        public ExtendedLambda ReadExtendedLambda (uint location)
        {
            string [] second = (string []) ReadFormals (location + 4);
            object third = ReadObject (location + 8);
            EncodedObject argcount = new EncodedObject ((uint) third);
            uint optional = (argcount.Datum & 0x00FF);
            uint required = (argcount.Datum & 0xFF00) >> 8;
            bool rest = ((argcount.Datum & 0x10000) == 0x10000);
            return new ExtendedLambda (ReadSCode (location), second, required, optional, rest);
        }

        //public Sequence3 ReadSequence3 (int location)
        //{
        //    return new Sequence3 (SCode.Convert (ReadObject (location)), SCode.Convert (ReadObject (location + 4)), SCode.Convert (ReadObject (location + 8)));
        //}


        internal SCode ReadSCode (uint location)
        {
            object obj = ReadObject (location);
            if (obj == null)
                return Quotation.Make (obj);
            SCode sobj = obj as SCode;
            if (sobj == null)
                return Quotation.Make (obj);
            return sobj;
        }

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
                //case TC.ACCESS:
                //    return ReadAccess (encoded.Datum);

                case TC.ASSIGNMENT:
                    return ReadAssignment (encoded.Datum);

                //case TC.BIG_FIXNUM:
                //    return ReadBignum (encoded.Datum);

                //case TC.BIG_FLONUM:
                //    return ReadBigFlonum (encoded.Datum);

                //case TC.CHARACTER:
                //    return (char) (encoded.Datum);

                case TC.CHARACTER_STRING:
                    return heapSection.ReadString (encoded.Datum);

                case TC.COMBINATION:
                    return new Combination (ReadSCodeVector (encoded.Datum));

                case TC.COMBINATION_1:
                    return new Combination1 (ReadSCode (encoded.Datum),
                                             ReadSCode (encoded.Datum + 4));

                case TC.COMBINATION_2:
                    return Combination2.Make (ReadSCode (encoded.Datum),
                                              ReadSCode (encoded.Datum + 4),
                                              ReadSCode (encoded.Datum + 8));

                case TC.COMMENT:
                    return new Comment (ReadSCode (encoded.Datum),
                                        ReadObject (encoded.Datum + 4));

                //case TC.COMPLEX:
                //    return new Complex (ReadObject (encoded.Datum),
                //                        ReadObject (encoded.Datum + 4));

                case TC.CONDITIONAL:
                    return Conditional.Make (ReadSCode (encoded.Datum),
                                             ReadSCode (encoded.Datum + 4),
                                             ReadSCode (encoded.Datum + 8));

                case TC.CONSTANT:
                    if (encoded.Datum == 0)
                        return sharpT;
                    else if (encoded.Datum == 1)
                        return Constant.Unspecific;
                    else
                        throw new NotImplementedException();

                case TC.DEFINITION:
                    return new Definition ((string) ReadObject (encoded.Datum),
                                            ReadSCode (encoded.Datum + 4));

                //case TC.DELAY:
                //    return new Delay (SCode.Convert (ReadObject (encoded.Datum)));

                case TC.DISJUNCTION:
                    return new Disjunction (ReadSCode (encoded.Datum),
                                            ReadSCode (encoded.Datum + 4));

                case TC.EXTENDED_LAMBDA:
                    return ReadExtendedLambda (encoded.Datum);

                case TC.FIXNUM:
                    return encoded.Datum > 0x02000000
                           ? encoded.Datum - 0x04000000
                           : encoded.Datum;

                case TC.INTERNED_SYMBOL:
                    return String.Intern (new String ((char []) ReadObject (encoded.Datum)));

                case TC.LAMBDA:
                    return new Lambda (ReadSCode (encoded.Datum), ReadFormals (encoded.Datum + 4));

                case TC.LIST:
                    return new Cons (ReadObject (encoded.Datum),
                                     ReadObject (encoded.Datum + 4));

                case TC.NULL:
                    if (encoded.Datum != 0)
                       throw new NotImplementedException();
                    return null;

                case TC.SEQUENCE_2:
                    return new Sequence2 (ReadSCode (encoded.Datum),
                                          ReadSCode (encoded.Datum + 4));

                case TC.SEQUENCE_3:
                    return new Sequence3 (ReadSCode (encoded.Datum),
                                          ReadSCode (encoded.Datum + 4),
                                          ReadSCode (encoded.Datum + 8));

                case TC.PCOMB0:
                    return new PrimitiveCombination0 ((Primitive0) primSection[encoded.Datum]);

                case TC.PCOMB1:
                    return PrimitiveCombination1.Make ((Primitive1) ReadObject (encoded.Datum),
                                                       ReadSCode (encoded.Datum + 4));

                case TC.PCOMB2:
                    return PrimitiveCombination2.Make ((Primitive2) ReadObject (encoded.Datum),
                                                        ReadSCode (encoded.Datum + 4),
                                                        ReadSCode (encoded.Datum + 8));

                case TC.PCOMB3:
                    return new PrimitiveCombination3 ((Primitive3) ReadObject (encoded.Datum + 4),
                                                       ReadSCode (encoded.Datum + 8),
                                                       ReadSCode (encoded.Datum + 12),
                                                       ReadSCode (encoded.Datum + 16));

                case TC.PRIMITIVE:
                    return primSection [encoded.Datum];

                case TC.REFERENCE_TRAP:
                    return ReferenceTrap.Make (encoded.Datum);

                //case TC.RETURN_CODE:
                //    return (ReturnCode) (encoded.Datum);

                //case TC.THE_ENVIRONMENT:
                //    return new TheEnvironment ();

                case TC.UNINTERNED_SYMBOL:
                    // KLUDGE!!  Make sure that all uninterned symbols within a file
                    // keep their identity when read.
                    first = ReadObject (encoded.Datum);
                    if (first is string)
                        return first;
                    else
                    {
                        string result = new String ((char []) first);
                        this.sharingTable.Add (encoded.Datum, result);
                        return result;
                    }

                case TC.VARIABLE:
                    return new Variable ((string) ReadObject (encoded.Datum));

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

        public static object Fasload (string pathName)
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
    }
}
