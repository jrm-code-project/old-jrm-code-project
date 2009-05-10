using System;
using System.Collections.Generic;
using System.Diagnostics;

namespace Microcode
{
    public enum TC
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
        RECORD = 0x3E,
        SPECIAL = 0x3F  // for magic objects
    }

    interface ISystemHunk3
    {
        object SystemHunk3Cxr0
        {
            get;
            set;
        }
        object SystemHunk3Cxr1
        {
            get;
            set;
        }
        object SystemHunk3Cxr2
        {
            get;
            set;
        }
    }
    
    interface ISystemVector
    {
        int SystemVectorSize
        {
            get;
        }

        object SystemVectorRef (int index);
        object SystemVectorSet (int index, object newValue);
    }

    sealed class ObjectModel
    {
        private ObjectModel ()
        {
        }

        [SchemePrimitive ("OBJECT-IS-ZERO?", 1, true)]
        public static bool ObjectIsZero (out object answer, object arg0)
        {
            // If arg0 is an integer and if it is zero.
            // like fix:zero?, but no error if arg is not an int.
            answer = (arg0 is int && (int) arg0 == 0) ? Constant.sharpT : Constant.sharpF;
            return false;
        }

        [SchemePrimitive ("OBJECT-IS-TRUE?", 1, true)]
        public static bool ObjectIsTrue (out object answer, object arg0)
        {
            // If arg0 is essentially EQ to sharpT.
            answer = (arg0 is bool && (bool) arg0) ? Constant.sharpT : Constant.sharpF;
            return false;
        }


        [SchemePrimitive ("CHAR-EQ?", 2, true)]
        public static bool CharEq (out object answer, object arg0, object arg1)
        {
            // For chars only.
            answer = (arg0 is char
                      && arg1 is char
                      && (char) arg0 == (char) arg1) ? Constant.sharpT : Constant.sharpF;
            return false;
        }

        [SchemePrimitive ("INT-EQ?", 2, true)]
        public static bool IntEq (out object answer, object arg0, object arg1)
        {
            // For integers only.
            answer = (arg0 is int
                      && arg1 is int
                      && (int) arg0 == (int) arg1) ? Constant.sharpT : Constant.sharpF;
            return false;
        }

        [SchemePrimitive ("BOOL-EQ?", 2, true)]
        public static bool BoolEq (out object answer, object arg0, object arg1)
        {
            // For booleans only.
            answer = (arg0 is bool
                      && arg1 is bool
                      && (bool) arg0 == (bool) arg1) ? Constant.sharpT : Constant.sharpF;
            return false;
        }

        [SchemePrimitive ("OBJECT-EQ?", 2, true)]
        public static bool ObjectEq (out object answer, object arg0, object arg1)
        {
            // For objects only.  Wrong for fixnums, etc.
            answer = arg0 == arg1 ? Constant.sharpT : Constant.sharpF;
            return false;
        }

        [SchemePrimitive ("EQ?", 2, true)]
        public static bool Eq (out object answer, object arg0, object arg1)
        {
            answer =
                ((arg0 == null) && (arg1 == null))
                || ((arg1 != null) &&
                    ((arg0 == arg1)
                     || ((arg0 is Int32 && arg1 is Int32) && ((int) arg0 == (int) arg1))
                     || ((arg0 is char && arg1 is char) && ((char) arg0 == (char) arg1))
                     || ((arg0 is bool && arg1 is bool) && ((bool) arg0 == (bool) arg1))))
                     ? Constant.sharpT
                     : Constant.sharpF;


            //answer =
            //    (arg0 == null) ? (arg1 == null)
            //    : (arg1 == null) ? false
            //    : (arg0 == arg1) ? true
            //    : 
            //    : 
            //    : (arg0 is bool && arg1 is bool) ? ((bool) arg0 == (bool) arg1)
            //    : false;
            return false;
        }

        [SchemePrimitive ("HUNK3-CONS", 3, true)]
        public static bool Hunk3Cons (out object answer, object cxr0, object cxr1, object cxr2)
        {
            answer = new Hunk3 (cxr0, cxr1, cxr2);
            return false;
        }

        [SchemePrimitive ("MAKE-NON-POINTER-OBJECT", 1, false)]
        public static bool MakeNonPointerObject (out object answer, object arg)
        {
            answer = arg;
            return false;
        }

        [SchemePrimitive ("NOT", 1, true)]
        public static bool Not (out object answer, object arg)
        {
            answer = ((arg is bool) && (bool) arg == false)
                ? Constant.sharpT
                : Constant.sharpF;
            return false;
        }

        [SchemePrimitive ("NULL?", 1, true)]
        public static bool IsNull (out object answer, object arg)
        {
            answer = (arg == null  ? Constant.sharpT : Constant.sharpF);
            return false;
        }

        // MIT Scheme uses the address of an object for various
        // things.  It knows the object is moved by the GC and 
        // synchronizes with it.  We can't do that.  Instead, we
        // keep a list of weak references to objects and then keep
        // another table to the `address' of the objects referred to.
        //static int objectDatumCounter = 42;
        //public static IDictionary<object, int> objectDatumDictionary = new Dictionary<object, int>();
        //public static IDictionary<int, object> datumObjectDictionary = new Dictionary<int, object> ();

        [SchemePrimitive ("OBJECT-DATUM", 1, true)]
        public static bool ObjectDatum (out object answer, object arg)
        {
            Primitive prim = arg as Primitive;
            // make.scm does this song and dance with a primitive to find
            // if it exists.  We just sidestep by hacking object-datum to
            // return the primitive itself!
            if (prim != null) {
                answer = prim;
            }

            else if (arg is bool && (bool) arg == false) {
                answer = 0;
            }
            else if (arg == null)
                answer = 9;
            else if (arg is Int32)
                answer = arg;
            else if (arg is long)
                answer = arg;

            ////else {
            ////    int probe;
            ////    if (objectDatumDictionary.TryGetValue (arg, out probe)) {
            ////        return probe);
            ////    }
            ////    int datum = objectDatumCounter;
            ////    objectDatumDictionary.Add (arg, datum);
            ////    datumObjectDictionary.Add (datum, arg);
            ////    objectDatumCounter += 1;
            ////    return datum);
            else if (arg is ReferenceTrap) {
                ReferenceTrap rarg = (ReferenceTrap) arg;
                TrapKind kind = ReferenceTrap.GetTrapKind (rarg);
                if (kind == TrapKind.TRAP_UNASSIGNED
                    || kind == TrapKind.TRAP_UNBOUND
                    || kind == TrapKind.TRAP_EXPENSIVE)
                    answer =  (int) kind;
                else {
                    answer =  arg.GetHashCode ();
                }
            }
            else
                answer = arg.GetHashCode ();
            if (answer is int && (int)answer < 0) Debugger.Break ();
            return false;
        }


        [SchemePrimitive ("OBJECT-GC-TYPE", 1, true)]
        public static bool ObjectGCType (out object answer, object arg)
        {
            // '#(COMPILED-ENTRY VECTOR GC-INTERNAL UNDEFINED NON-POINTER
            //	CELL PAIR TRIPLE QUADRUPLE)   
            if (arg is ISystemVector
                            || arg is object []
                || arg is char [])
                answer = -3;
            else if (arg == null
                            || arg is bool
                            || arg is char
                            || arg is double
                            || arg is int
                            || arg is long
                            || arg is Constant)
                answer = 0;
            else if (arg is ISystemPair)
                answer = 2;
            else if (arg is ISystemHunk3)
                answer = 3;
            else
                throw new NotImplementedException ("Object-gc-type: " + arg.ToString ());
            return false;
        }

        [SchemePrimitive ("OBJECT-TYPE", 1, true)]
        public static bool ObjectType (out object answer, object arg)
        {
            return PrimitiveObjectType (out answer, arg);
        }

        [SchemePrimitive ("OBJECT-TYPE?", 2, false)]
        public static bool IsObjectType (out object answer, object arg0, object arg1)
        {
            return IsPrimitiveObjectType (out answer, arg0, arg1);
        }


        [SchemePrimitive ("OBJECT-SET-TYPE", 2, false)]
        public static bool ObjectSetType (out object answer, object arg0, object arg1)
        {
            TC newType = (TC) (int) arg0;
            // kludge!!!!
            if ((int) arg0 == 0 && (int) arg1 == 1)
                answer = new NullEnvironment (null);
            else
            switch (newType)
            {
                case TC.COMBINATION_2:
                    answer = Combination2.Make ((Hunk3) arg1);
                    break;

                case TC.CONDITIONAL:
                    answer = Conditional.Make ((Hunk3) arg1);
                    break;

                case TC.CONSTANT:
                    answer = Constant.Decode ((uint) (int) arg1);
                    break;

                case TC.HUNK3_A:
                    // Probably someone trying to mark a history object.
                    answer = arg1;
                    break;

                case TC.HUNK3_B:
                    answer = arg1;
                    break;

                case TC.ENVIRONMENT:
                    object [] args = (object[]) arg1;
                    ClosureBase closure = (ClosureBase) args [0];
                    object [] actualArgs = new object [args.Length - 1];
                    for (int i = 0; i < actualArgs.Length; i++)
                        actualArgs [i] = args [i + 1];
                    answer = new StandardEnvironment (closure, actualArgs);
                    break;

                     // throw new NotImplementedException ();
        //            // answer = (new InterpreterEnvironment ((object []) arg1));

                case TC.EXTENDED_LAMBDA:
                    answer = ExtendedLambda.Make ((Hunk3) arg1);
                    break;

                case TC.PCOMB2:
                    answer = PrimitiveCombination2.Make ((Hunk3) arg1);
                    break;

                case TC.RECORD:
                    answer = new Record ((object []) arg1);
                    return false;

                case TC.SEQUENCE_3:
                    answer = new Sequence3 ((Hunk3) arg1);
                    break;

                case TC.THE_ENVIRONMENT:
                    answer = TheEnvironment.Make();
                    break;

                case TC.VARIABLE:
                    answer =  Variable.Make ((Hunk3) arg1);
                    break;

                case TC.VECTOR:
                    // Someone wants to see what endian we are! 
                    char [] source = (char []) arg1;
                    object [] result = new object [source.Length / 4];
                    result [1] = ((((((byte) source [3]) * 256)
                        + ((byte) source [2])) * 256)
                        + ((byte) source [1])) * 256
                        + ((byte) source [0]);
                    result [0] = ((((((byte) source [7]) * 256)
                                        + ((byte) source [6])) * 256)
                                        + ((byte) source [5])) * 256
                                        + ((byte) source [4]);
                    answer = result;
                    break;

                case TC.WEAK_CONS:
                    answer = new WeakCons (((Cons) arg1).Car, ((Cons) arg1).Cdr);
                    break;

               default:
                    throw new NotImplementedException ();
            }
            return false;
        }

        [SchemePrimitive ("PRIMITIVE-OBJECT-EQ?", 2, true)]
        public static bool PrimitiveObjectEq (out object answer, object arg0, object arg1)
        {
            answer = (Object.ReferenceEquals (arg0, arg1));
            return false;
        }

        [SchemePrimitive ("PRIMITIVE-OBJECT-TYPE", 1, true)]
        public static bool PrimitiveObjectType (out object answer, object arg)
        {
            if (arg == null)
                answer =  (int) TC.CONSTANT;
            else if (arg is object [])
                answer = (int) TC.VECTOR;
            else if (arg is Boolean)
                answer = (bool) arg == false ? (int) TC.NULL : (int) TC.CONSTANT;
            else if (arg is char)
                answer = (int) TC.CHARACTER;
            else if (arg is char [])
                answer = (int) TC.CHARACTER_STRING;
            else if (arg is double)
                answer = (int) TC.BIG_FLONUM;
            else if (arg is int || arg is long)
                answer = (int) TC.FIXNUM;
            else if (arg is SchemeObject)
                answer = (int) ((SchemeObject) arg).TypeCode;
            else
                throw new NotImplementedException ();
            return false;
        }


        [SchemePrimitive ("PRIMITIVE-OBJECT-TYPE?", 2, false)]
        public static bool IsPrimitiveObjectType (out object answer, object arg0, object arg1)
        {
            TC targetType = (TC) arg0;
            bool banswer;
            switch (targetType) {
                case TC.ACCESS:
                    banswer = arg1 is Access;
                    break;

                case TC.ASSIGNMENT:
                    banswer = arg1 is Assignment;
                    break;

                case TC.BIG_FIXNUM:
                    banswer = arg1 is Int64;
                    break;

                case TC.BIG_FLONUM:
                    banswer = arg1 is double;
                    break;

                case TC.BROKEN_HEART:
                    banswer = false;
                    break;

                case TC.CHARACTER:
                    banswer = arg1 is char;
                    break;

                case TC.CHARACTER_STRING:
                    banswer = arg1 is char [];
                    break;

                case TC.COMBINATION:
                    banswer = arg1 is Combination || arg1 is Combination0 || arg1 is Combination3 || arg1 is Combination4;
                    break;

                case TC.COMBINATION_1:
                    banswer = arg1 is Combination1;
                    break;

                case TC.COMBINATION_2:
                    banswer = arg1 is Combination2;
                    break;

                case TC.COMMENT:
                    banswer = arg1 is Comment;
                    break;

                case TC.COMPILED_ENTRY:
                    banswer = false;
                    break;

                case TC.COMPLEX:
                    banswer = arg1 is Complex;
                    break;

                case TC.CONDITIONAL:
                    banswer = arg1 is Conditional;
                    break;

                case TC.CONSTANT:
                    banswer = arg1 == null
                           || arg1 is Constant
                           || ((arg1 is bool) && ((bool) arg1 == true));
                    break;

                case TC.DEFINITION:
                    banswer = arg1 is Definition;
                    break;

                case TC.DELAY:
                    banswer = arg1 is Delay;
                    break;

                case TC.DELAYED:
                    banswer = arg1 is Promise;
                    break;

                case TC.DISJUNCTION:
                    banswer = arg1 is Disjunction;
                    break;

                case TC.ENTITY:
                    banswer = arg1 is Entity;
                    break;

                case TC.ENVIRONMENT:
                    banswer = arg1 is Environment;
                    break;

                case TC.EXTENDED_LAMBDA:
                    banswer = arg1 is ExtendedLambda;
                    break;

                case TC.EXTENDED_PROCEDURE:
                    banswer = arg1 is ExtendedClosure;
                    break;

                case TC.FIXNUM:
                    banswer = arg1 is Int32;
                    break;

                case TC.FUTURE:
                    banswer = false;
                    break;

                case TC.HUNK3_B:
                    banswer = arg1 is MarkedHistory;
                    break;

                case TC.INTERNED_SYMBOL:
                    banswer = arg1 is Symbol && ((Symbol)arg1).TypeCode == TC.INTERNED_SYMBOL;
                    break;

                case TC.LAMBDA:
                    banswer = arg1 is Lambda;
                    break;

                case TC.LIST:
                    banswer = arg1 is Cons;
                    break;

                case TC.LEXPR:
                    banswer = false;
                    break;

                case TC.MANIFEST_NM_VECTOR:
                    banswer = false;
                    break;

                case TC.NULL:
                    banswer = (arg1 is bool) && ((bool) arg1 == false);
                    break;

                case TC.PCOMB0:
                    banswer = arg1 is PrimitiveCombination0;
                    break;

                case TC.PCOMB1:
                    banswer = arg1 is PrimitiveCombination1;
                    break;

                case TC.PCOMB2:
                    banswer = arg1 is PrimitiveCombination2;
                    break;

                case TC.PCOMB3:
                    banswer = arg1 is PrimitiveCombination3;
                    break;

                case TC.PRIMITIVE:
                    banswer = arg1 is Primitive;
                    break;

                case TC.PROCEDURE:
                    banswer = arg1 is Closure;
                    break;

                case TC.RATNUM:
                    banswer = arg1 is Ratnum;
                    break;

                case TC.RECORD:
                    banswer = arg1 is Record;
                    break;

                case TC.REFERENCE_TRAP:
                    banswer = arg1 is ReferenceTrap;
                    break;

                case TC.RETURN_CODE:
                    banswer = arg1 is ReturnCode;
                    break;

                case TC.SEQUENCE_2:
                    banswer = arg1 is Sequence2;
                    break;

                case TC.SEQUENCE_3:
                    banswer = arg1 is Sequence3;
                    break;

                case TC.UNINTERNED_SYMBOL:
                    banswer = arg1 is Symbol && ((Symbol) arg1).TypeCode == TC.UNINTERNED_SYMBOL;
                    break;

                case TC.VARIABLE:
                    banswer = arg1 is Variable;
                    break;

                case TC.VECTOR:
                    banswer = arg1 is object [];
                    break;

                case TC.WEAK_CONS:
                    banswer = arg1 is WeakCons;
                    break;

                default:
                    throw new NotImplementedException ();
            }
            answer = banswer ? Constant.sharpT : Constant.sharpF;
            return false;
        }

        [SchemePrimitive ("PRIMITIVE-OBJECT-REF", 2, false)]
        public static bool PrimitiveObjectRef (ref object answer, object arg0, object arg1)
        {
            ReferenceTrap reftrap = arg0 as ReferenceTrap;
            if (reftrap != null) {
                int idx = (int) arg1;
                if (idx == 0)
                    answer = (((Cons) (reftrap.Contents)).Car);
                else if (idx == 1)
                    answer = (((Cons) (reftrap.Contents)).Cdr);
                else
                    throw new NotImplementedException ();
            }
            else
                throw new NotImplementedException ();
            return false;
        }


        [SchemePrimitive ("PRIMITIVE-OBJECT-SET-TYPE", 2, false)]
        public static bool PrimitiveObjectSetType (out object answer, object arg0, object arg1)
        {
            TC newType = (TC) (int) arg0;
            switch (newType) {

                case TC.FIXNUM:

                    answer = (arg1 == null) ? 9
                        : (arg1 is SchemeObject) ? (int)((SchemeObject)arg1).GetHashCode()
                        : arg1.GetHashCode();
                    break;

                case TC.MANIFEST_NM_VECTOR:
                    answer = arg1;
                    break;

                case TC.MANIFEST_SPECIAL_NM_VECTOR:
                    answer = new ManifestSpecialNMVector ((int) arg1);
                    break;

                case TC.NON_MARKED_VECTOR:
                    answer = new NonMarkedVector (arg1);
                    break;

                case TC.REFERENCE_TRAP:
                    answer = ReferenceTrap.Make (arg1);
                    break;

                case TC.VECTOR:
                    answer = ((NonMarkedVector) arg1).contents;
                    break;

                default:
                    throw new NotImplementedException ();
            }
            return false;
        }

        [SchemePrimitive ("SYMBOL?", 1, true)]
        public static bool IsSymbol (out object answer, object arg)
        {
            answer = (arg is Symbol && ((Symbol) arg).TypeCode == TC.INTERNED_SYMBOL) ? Constant.sharpT : Constant.sharpF;
            return false;
        }

        [SchemePrimitive ("UNINTERNED-SYMBOL?", 1, true)]
        public static bool IsUninternedSymbol (out object answer, object arg)
        {
            answer = (arg is Symbol && ((Symbol) arg).TypeCode == TC.UNINTERNED_SYMBOL) ? Constant.sharpT : Constant.sharpF;
            return false;
        }

        [SchemePrimitive ("SYSTEM-HUNK3-CXR0", 1, false)]
        public static bool SystemHunk3Cxr0 (out object answer, object arg)
        {
            ISystemHunk3 hunk = arg as ISystemHunk3;
            if (hunk != null)
                answer = hunk.SystemHunk3Cxr0;
            else

                throw new NotImplementedException ();
            return false;
        }

        [SchemePrimitive ("SYSTEM-HUNK3-CXR1", 1, false)]
        public static bool SystemHunk3Cxr1 (out object answer, object arg)
        {
            ISystemHunk3 hunk = arg as ISystemHunk3;
            if (hunk != null)
                answer = hunk.SystemHunk3Cxr1;
            else

                throw new NotImplementedException ();
            return false;
        }

        [SchemePrimitive ("SYSTEM-HUNK3-CXR2", 1, false)]
        public static bool SystemHunk3Cxr2 (out object answer, object arg)
        {
            ISystemHunk3 hunk = arg as ISystemHunk3;
            if (hunk != null)
                answer = hunk.SystemHunk3Cxr2;
            else

                throw new NotImplementedException ();
            return false;
        }

        [SchemePrimitive ("SYSTEM-HUNK3-SET-CXR0!", 2, false)]
        public static bool SystemHunk3SetCxr0 (out object answer, object arg, object newValue)
        {
            ISystemHunk3 hunk = arg as ISystemHunk3;
            if (hunk != null) {
                answer = hunk.SystemHunk3Cxr0;
                hunk.SystemHunk3Cxr0 = newValue;
                return false;
            }
            else

                throw new NotImplementedException ();
        }
        [SchemePrimitive ("SYSTEM-HUNK3-SET-CXR1!", 2, false)]
        public static bool SystemHunk3SetCxr1 (out object answer, object arg, object newValue)
        {
            ISystemHunk3 hunk = arg as ISystemHunk3;
            if (hunk != null) {
                answer = hunk.SystemHunk3Cxr1;
                hunk.SystemHunk3Cxr1 = newValue;
                return false;
            }
            else

                throw new NotImplementedException ();
        }
        [SchemePrimitive ("SYSTEM-HUNK3-SET-CXR2!", 2, false)]
        public static bool SystemHunk3SetCxr2 (out object answer, object arg, object newValue)
        {
            ISystemHunk3 hunk = arg as ISystemHunk3;
            if (hunk != null) {
                answer = hunk.SystemHunk3Cxr2;
                hunk.SystemHunk3Cxr2 = newValue;
                return false;
            }
            else

                throw new NotImplementedException ();
        }

        [SchemePrimitive ("SYSTEM-LIST-TO-VECTOR", 2, false)]
        public static bool SystemListToVector (out object answer, object arg0, object arg1)
        {
            TC code = (TC) arg0;
            switch ((TC) arg0) {
                case TC.COMBINATION:
                    answer = Combination.FromList ((Cons) arg1);
                    break;

                case TC.ENVIRONMENT:
                    ClosureBase closure = (ClosureBase) ((Cons) arg1).Car;
                    object tail = ((Cons) arg1).Cdr;
                    object [] initialValues = 
                        tail == null 
                        ? new object [0]
                        : ((Cons) tail).ToVector ();
                    answer = new StandardEnvironment (closure, initialValues);
                    break;

                case TC.PCOMB3:
                    answer = PrimitiveCombination3.Make ((Cons) arg1);
                    break;

                default:
                    throw new NotImplementedException ();
            }
            return false;
        }



        [SchemePrimitive ("SYSTEM-SUBVECTOR-TO-LIST", 3, false)]
        public static bool SystemSubvectorToList (out object answer, object arg, object start, object end)
        {
            ISystemVector sysVec = arg as ISystemVector;
            if (sysVec != null) {
                Cons result = null;
                for (int i = (int) end - 1; i > ((int) start - 1); i--) {
                    result = new Cons (sysVec.SystemVectorRef (i), result);
                }
                answer = result;
                return false;
            }
            else throw new NotImplementedException ();
        }

        [SchemePrimitive ("SYSTEM-VECTOR-REF", 2, false)]
        public static bool SystemVectorRef (out object answer, object arg, object offset)
        {
            ISystemVector sysVec = arg as ISystemVector;
            if (sysVec != null)
                answer = sysVec.SystemVectorRef ((int) offset);
            else
                throw new NotImplementedException ();
            return false;
        }

        [SchemePrimitive ("SYSTEM-VECTOR-SIZE", 1, false)]
        public static bool SystemVectorSize (out object answer, object arg)
        {
            ISystemVector sysVec = arg as ISystemVector;
            if (sysVec != null)
                answer = sysVec.SystemVectorSize;
            else if (arg is bool [])
                answer = (((bool []) arg).Length / 32) + 1;
            else if (arg is int)
                answer = 0x1000;
            else if (arg is long)
                answer = 0x2000;
            else if (arg is double)
                answer = 2;
            else
                throw new NotImplementedException ();
            return false;
        }
    }
}
