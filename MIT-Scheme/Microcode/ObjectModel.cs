using System;
using System.Collections.Generic;

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
        RECORD = 0x3E
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
    
    interface ISystemPair
    {
        object SystemPairCar
        {
            get;
            set;
        }

        object SystemPairCdr
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

        [SchemePrimitive ("EQ?", 2)]
        public static object Eq (Interpreter interpreter, object arg0, object arg1)
        {
            if (arg0 == null)
                return interpreter.Return (arg1 == null);
            else if (arg1 == null)
                return interpreter.Return (false);
            else if (Object.ReferenceEquals (arg0, arg1))
                return interpreter.Return (true);
            else if (arg0 is Int32 && arg1 is Int32)
                return interpreter.Return ((int) arg0 == (int) arg1);
            else if (arg0 is char && arg1 is char)
                return interpreter.Return ((char) arg0 == (char) arg1);
            else if (arg0 is bool && arg1 is bool)
                return interpreter.Return ((bool) arg0 == (bool) arg1);
            else
                return interpreter.Return (false);
        }

        [SchemePrimitive ("MAKE-NON-POINTER-OBJECT", 1)]
        public static object MakeNonPointerObject (Interpreter interpreter, object arg)
        {
            return interpreter.Return (arg);
        }

        [SchemePrimitive ("NOT", 1)]
        public static object Not (Interpreter interpreter, object arg)
        {
            if (arg is bool)
            {
                bool val = (bool) arg;
                if (val == false)
                    return interpreter.Return (true);
                else
                    return interpreter.Return (false);
            }
            else
                return interpreter.Return (false);

        }

        [SchemePrimitive ("NULL?", 1)]
        public static object IsNull (Interpreter interpreter, object arg)
        {
            return interpreter.Return (arg == null);
        }

        // MIT Scheme uses the address of an object for various
        // things.  It knows the object is moved by the GC and 
        // synchronizes with it.  We can't do that.  Instead, we
        // keep a list of weak references to objects and then keep
        // another table to the `address' of the objects referred to.
        //static int objectDatumCounter = 42;
        //public static IDictionary<object, int> objectDatumDictionary = new Dictionary<object, int>();
        //public static IDictionary<int, object> datumObjectDictionary = new Dictionary<int, object> ();

        [SchemePrimitive ("OBJECT-DATUM", 1)]
        public static object ObjectDatum (Interpreter interpreter, object arg)
        {
            Primitive prim = arg as Primitive;
            // make.scm does this song and dance with a primitive to find
            // if it exists.  We just sidestep by hacking object-datum to
            // return the primitive itself!
            if (prim != null) {
                return interpreter.Return(prim);
            }
            else  if (arg is bool && (bool) arg == false)
                return interpreter.Return (0);
            else if (arg == null)
                return interpreter.Return (9);
            else if (arg is Int32)
                return interpreter.Return (arg);
            //else {
            //    int probe;
            //    if (objectDatumDictionary.TryGetValue (arg, out probe)) {
            //        return interpreter.Return (probe);
            //    }
            //    int datum = objectDatumCounter;
            //    objectDatumDictionary.Add (arg, datum);
            //    datumObjectDictionary.Add (datum, arg);
            //    objectDatumCounter += 1;
            //    return interpreter.Return (datum);
            else {
                return interpreter.Return (arg.GetHashCode ());
            }
            
        }

        [SchemePrimitive("OBJECT-GC-TYPE", 1)]
        public static object ObjectGCType(Interpreter interpreter, object arg)
        {
             //Primitive.Noisy = false;
            if (arg == null)
                return interpreter.Return (0);
            else if (arg is object [])
                return interpreter.Return (-3);
            else if (arg is char [])
                return interpreter.Return (0);
            else if (arg is int)
                return interpreter.Return (0);
            else if (arg is string)
                return interpreter.Return (0);
            else if (arg is ISystemPair)
                return interpreter.Return (2);
            else if (arg is Quotation)
                return interpreter.Return (1);
            // '#(COMPILED-ENTRY VECTOR GC-INTERNAL UNDEFINED NON-POINTER
            //	CELL PAIR TRIPLE QUADRUPLE)
            else if (arg is Boolean ||
                     arg is char ||
                     arg is Constant)
                return interpreter.Return (0);
            else
                throw new NotImplementedException ();
        }

        [SchemePrimitive ("OBJECT-TYPE", 1)]
        public static object ObjectType (Interpreter interpreter, object arg)
        {
            if (arg == null)
                return interpreter.Return (TC.NULL);
            else if (arg is bool)
                return interpreter.Return (TC.CONSTANT);
            else if (arg is char)
                return interpreter.Return (TC.CHARACTER);
            else if (arg is char [])
                return interpreter.Return (TC.CHARACTER_STRING);
            else if (arg is Access)
                return interpreter.Return (TC.ACCESS);
            else if (arg is Assignment)
                return interpreter.Return (TC.ASSIGNMENT);
            else if (arg is Closure)
                return interpreter.Return (TC.PROCEDURE);
            else if (arg is Definition)
                return interpreter.Return (TC.DEFINITION);
            else if (arg is Combination)
                return interpreter.Return (TC.COMBINATION);
            else if (arg is Combination1)
                return interpreter.Return (TC.COMBINATION_1);
            else if (arg is Combination2)
                return interpreter.Return (TC.COMBINATION_2);
            else if (arg is Conditional)
                return interpreter.Return (TC.CONDITIONAL);
            else if (arg is Cons)
                return interpreter.Return (TC.LIST);
            else if (arg is Constant)
                return interpreter.Return (TC.CONSTANT);
            else if (arg is ExtendedLambda)
                return interpreter.Return (TC.EXTENDED_LAMBDA);
            else if (arg is int)
                return interpreter.Return (TC.FIXNUM);
            else if (arg is Lambda)
                return interpreter.Return (TC.LAMBDA);
            else if (arg is Primitive)
                return interpreter.Return (TC.PRIMITIVE);
            else if (arg is PrimitiveCombination2)
                return interpreter.Return (TC.PCOMB2);
            else if (arg is Promise)
                return interpreter.Return (TC.DELAYED);
            else if (arg is Quotation)
                return interpreter.Return (TC.SCODE_QUOTE);
            else if (arg is Sequence2)
                return interpreter.Return (TC.SEQUENCE_2);
            else if (arg is Sequence3)
                return interpreter.Return (TC.SEQUENCE_3);
            else if (arg is string)
                //return interpreter.Return (Misc.IsGensym ((string) arg) ? TC.UNINTERNED_SYMBOL : TC.INTERNED_SYMBOL);
                return interpreter.Return (TC.INTERNED_SYMBOL);
            else if (arg is Variable)
                return interpreter.Return (TC.VARIABLE);

            else
                throw new NotImplementedException ();
        }


        [SchemePrimitive ("OBJECT-TYPE?", 2)]
        public static object IsObjectType (Interpreter interpreter, object arg0, object arg1)
        {
            TC targetType = (TC) arg0;
            switch (targetType)
            {
                case TC.ACCESS:
                    return interpreter.Return (arg1 is Access);
                case TC.BIG_FIXNUM:
                    return interpreter.Return (arg1 is Int64);
                case TC.BIG_FLONUM:
                    return interpreter.Return (arg1 is double);
                case TC.BROKEN_HEART:
                    return interpreter.Return (false);
                case TC.CHARACTER:
                    return interpreter.Return (arg1 is char);
                case TC.CHARACTER_STRING:
                    return interpreter.Return (arg1 is char []);
                case TC.COMBINATION:
                    return interpreter.Return (arg1 is Combination);
                case TC.COMBINATION_1:
                    return interpreter.Return (arg1 is Combination1);
                case TC.COMBINATION_2:
                    return interpreter.Return (arg1 is Combination2);
                case TC.COMMENT:
                    return interpreter.Return (arg1 is Comment);
                case TC.COMPILED_ENTRY:
                    return interpreter.Return (false);
                case TC.COMPLEX:
                    return interpreter.Return (arg1 is Complex);
                case TC.CONDITIONAL:
                    return interpreter.Return(arg1 is Conditional);
                case TC.DEFINITION:
                    return interpreter.Return(arg1 is Definition);
                case TC.DELAY:
                    return interpreter.Return(arg1 is Delay);
                case TC.DELAYED:
                    return interpreter.Return (arg1 is Promise);
                case TC.DISJUNCTION:
                    return interpreter.Return(arg1 is Disjunction);
                case TC.ENTITY:
                    return interpreter.Return (arg1 is Entity);
                case TC.ENVIRONMENT:
                    return interpreter.Return (arg1 is Environment);
                case TC.EXTENDED_LAMBDA:
                    return interpreter.Return(arg1 is ExtendedLambda);
                case TC.EXTENDED_PROCEDURE:
                    return interpreter.Return (arg1 is ExtendedClosure);
                case TC.FIXNUM:
                    return interpreter.Return (arg1 is Int32);
                case TC.FUTURE:
                    return interpreter.Return (false);
                case TC.HUNK3_B:
                    return interpreter.Return (arg1 is MarkedHistory);
                case TC.INTERNED_SYMBOL:
                    return interpreter.Return (arg1 is string);
                //return interpreter.Return ((arg1 is string)
                //&& (!Misc.IsGensym ((string) arg1)));
                case TC.LAMBDA:
                    return interpreter.Return (arg1 is Lambda && ! (arg1 is ExtendedLambda));
                case TC.LIST:
                    return interpreter.Return (arg1 is Cons);
                case TC.LEXPR:
                    return interpreter.Return(false);
                case TC.MANIFEST_NM_VECTOR:
                    return interpreter.Return (false);
                case TC.NULL:
                    return interpreter.Return (arg1 == null);
                case TC.PCOMB0:
                    return interpreter.Return (arg1 is PrimitiveCombination0);
                case TC.PCOMB1:
                    return interpreter.Return (arg1 is PrimitiveCombination1);
                case TC.PCOMB2:
                    return interpreter.Return (arg1 is PrimitiveCombination2);
                case TC.PCOMB3:
                    return interpreter.Return (arg1 is PrimitiveCombination3);
                case TC.PRIMITIVE:
                    return interpreter.Return (arg1 is Primitive);
                case TC.PROCEDURE:
                    return interpreter.Return (arg1 is Closure);
                case TC.RATNUM:
                    return interpreter.Return (arg1 is Ratnum);
                case TC.REFERENCE_TRAP:
                    throw new NotImplementedException ();
                case TC.RETURN_CODE:
                    return interpreter.Return (arg1 is ReturnCode);
                case TC.SEQUENCE_2:
                    return interpreter.Return (arg1 is Sequence2);
                case TC.SEQUENCE_3:
                    return interpreter.Return (arg1 is Sequence3);
                case TC.UNINTERNED_SYMBOL:
                    return interpreter.Return ((arg1 is string)
                    && (Misc.IsGensym ((string) arg1)));
                case TC.VECTOR:
                    return interpreter.Return (arg1 is object []);
                case TC.WEAK_CONS:
                    return interpreter.Return(arg1 is WeakCons);
                default:
                    throw new NotImplementedException ();
            }
        }

        [SchemePrimitive ("OBJECT-SET-TYPE", 2)]
        public static object ObjectSetType (Interpreter interpreter, object arg0, object arg1)
        {
            TC newType = (TC) (int) arg0;
            // kludge!!!!
            if ((int) arg0 == 0 && (int) arg1 == 1)
                return interpreter.Return (new NullEnvironment ());
            switch (newType)
            {
                case TC.COMBINATION_2:
                    return interpreter.Return (new Combination2 ((Hunk3) arg1));

                case TC.CONDITIONAL:
                    return interpreter.Return (Conditional.Make ((Hunk3) arg1));

                case TC.CONSTANT:
                    return interpreter.Return (Constant.Decode ((uint) (int) arg1));

                case TC.HUNK3_A:
                    // Probably someone trying to mark a history object.
                    return interpreter.Return (arg1);
                case TC.HUNK3_B:
                    return interpreter.Return (arg1);
                case TC.ENVIRONMENT:
                    return interpreter.Return (new InterpreterEnvironment ((object []) arg1));
                case TC.EXTENDED_LAMBDA:
                    return interpreter.Return (new ExtendedLambda ((Hunk3) arg1));
                case TC.RECORD:
                    return interpreter.Return (new Record ((object []) arg1));
                case TC.SEQUENCE_3:
                    return interpreter.Return (new Sequence3 ((Hunk3) arg1));
                case TC.VARIABLE:
                    return interpreter.Return (new Variable ((Hunk3) arg1));
                case TC.VECTOR:
                    // Someone wants to see what endian we are! 
                    char [] source = (char []) arg1;
                    object [] result = new object [source.Length / 4];
                    result [1] = ((((((byte)source[3]) * 256) 
                        + ((byte)source[2])) * 256)
                    + ((byte)source[1])) * 256 
                        + ((byte)source[0]);
                                        result [0] = ((((((byte)source[7]) * 256) 
                        + ((byte)source[6])) * 256)
                    + ((byte)source[5])) * 256 
                        + ((byte)source[4]);
                    return interpreter.Return (result);
                case TC.WEAK_CONS:
                    return interpreter.Return (new WeakCons (((Cons) arg1).Car, ((Cons) arg1).Cdr));
                default:
                    throw new NotImplementedException ();
            }
        }

        [SchemePrimitive ("PRIMITIVE-OBJECT-EQ?", 2)]
        public static object PrimitiveObjectEq (Interpreter interpreter, object arg0, object arg1)
        {
            return interpreter.Return (Object.ReferenceEquals (arg0, arg1));
        }

        [SchemePrimitive ("PRIMITIVE-OBJECT-TYPE", 1)]
        public static object PrimitiveObjectType (Interpreter interpreter, object arg)
        {
            if (arg is object [])
                return interpreter.Return (TC.VECTOR);
            else if (arg is Boolean)
                return interpreter.Return ((bool)arg == false ? TC.NULL : TC.CONSTANT);
            else if (arg is char)
                return interpreter.Return(TC.CHARACTER);
            else if (arg is char[])
                return interpreter.Return(TC.CHARACTER_STRING);
            else if (arg is double)
                return interpreter.Return(TC.BIG_FLONUM);
            else if (arg is Closure)
                return interpreter.Return(TC.PROCEDURE);
            else if (arg is Cons)
                return interpreter.Return(TC.LIST);
            else if (arg is int)
                return interpreter.Return(TC.FIXNUM);
            else if (arg == null)
                return interpreter.Return(TC.CONSTANT);
            else if (arg is Constant)
                return interpreter.Return(TC.CONSTANT);
            else if (arg is string)
                return interpreter.Return(Misc.IsGensym((string)arg) ? TC.UNINTERNED_SYMBOL : TC.INTERNED_SYMBOL);
            else if (arg is TC)
                return interpreter.Return(TC.INTERNED_SYMBOL);
            else if (arg is Quotation)
                return interpreter.Return(TC.SCODE_QUOTE);
            else
                throw new NotImplementedException();
        }


        [SchemePrimitive ("PRIMITIVE-OBJECT-TYPE?", 2)]
        public static object IsPrimitiveObjectType (Interpreter interpreter, object arg0, object arg1)
        {
            TC targetType = (TC) arg0;
            switch (targetType)
            {
                case TC.MANIFEST_NM_VECTOR:
                    return interpreter.Return (false);
                case TC.REFERENCE_TRAP:
                    return interpreter.Return (arg1 is ReferenceTrap);
                default:
                    throw new NotImplementedException ();
            }
        }

        [SchemePrimitive ("PRIMITIVE-OBJECT-REF", 2)]
        public static object PrimitiveObjectRef (Interpreter interpreter, object arg0, object arg1)
        {
            ReferenceTrap reftrap = arg0 as ReferenceTrap;
            if (reftrap != null) {
                int idx = (int) arg1;
                if (idx == 0)
                    return interpreter.Return (((Cons) (reftrap.Contents)).Car);
                else if (idx == 1)
                    return interpreter.Return (((Cons) (reftrap.Contents)).Cdr);
                else
                    throw new NotImplementedException ();
            }
            else
                throw new NotImplementedException ();
        }


        [SchemePrimitive ("PRIMITIVE-OBJECT-SET-TYPE", 2)]
        public static object PrimitiveObjectSetType (Interpreter interpreter, object arg0, object arg1)
        {
            TC newType = (TC) (int) arg0;
            switch (newType)
            {
                case TC.FIXNUM:
                    return interpreter.Return (arg1.GetHashCode ());
                case TC.MANIFEST_NM_VECTOR:
                    return interpreter.Return (arg1);
                case TC.MANIFEST_SPECIAL_NM_VECTOR:
                    return interpreter.Return (new ManifestSpecialNMVector ((int) arg1));
                case TC.NON_MARKED_VECTOR:
                    return interpreter.Return (new NonMarkedVector (arg1));
                case TC.REFERENCE_TRAP:
                    return interpreter.Return (ReferenceTrap.Make (arg1));
                case TC.VECTOR:
                    return interpreter.Return (((NonMarkedVector)arg1).contents);
                default:
                    throw new NotImplementedException ();
            }
        }

        [SchemePrimitive ("SYSTEM-HUNK3-CXR0", 1)]
        public static object SystemHunk3Cxr0 (Interpreter interpreter, object arg)
        {
            ISystemHunk3 hunk = arg as ISystemHunk3;
            if (hunk != null)
                return interpreter.Return (hunk.SystemHunk3Cxr0);
            else

                throw new NotImplementedException ();
        }

        [SchemePrimitive ("SYSTEM-HUNK3-CXR1", 1)]
        public static object SystemHunk3Cxr1 (Interpreter interpreter, object arg)
        {
            ISystemHunk3 hunk = arg as ISystemHunk3;
            if (hunk != null)
                return interpreter.Return (hunk.SystemHunk3Cxr1);
            else

                throw new NotImplementedException ();
        }

        [SchemePrimitive ("SYSTEM-HUNK3-CXR2", 1)]
        public static object SystemHunk3Cxr2 (Interpreter interpreter, object arg)
        {
            ISystemHunk3 hunk = arg as ISystemHunk3;
            if (hunk != null)
                return interpreter.Return (hunk.SystemHunk3Cxr2);
            else

                throw new NotImplementedException ();
        }

        [SchemePrimitive ("SYSTEM-HUNK3-SET-CXR0!", 2)]
        public static object SystemHunk3SetCxr0 (Interpreter interpreter, object arg, object newValue)
        {
            ISystemHunk3 hunk = arg as ISystemHunk3;
            if (hunk != null)
            {
                object oldValue = hunk.SystemHunk3Cxr0;
                hunk.SystemHunk3Cxr0 = newValue;
                return interpreter.Return (oldValue);
            }
            else

                throw new NotImplementedException ();
        }
        [SchemePrimitive ("SYSTEM-HUNK3-SET-CXR1!", 2)]
        public static object SystemHunk3SetCxr1 (Interpreter interpreter, object arg, object newValue)
        {
            ISystemHunk3 hunk = arg as ISystemHunk3;
            if (hunk != null)
            {
                object oldValue = hunk.SystemHunk3Cxr1;
                hunk.SystemHunk3Cxr1 = newValue;
                return interpreter.Return (oldValue);
            }
            else

                throw new NotImplementedException ();
        }
        [SchemePrimitive ("SYSTEM-HUNK3-SET-CXR2!", 2)]
        public static object SystemHunk3SetCxr2 (Interpreter interpreter, object arg, object newValue)
        {
            ISystemHunk3 hunk = arg as ISystemHunk3;
            if (hunk != null)
            {
                object oldValue = hunk.SystemHunk3Cxr2;
                hunk.SystemHunk3Cxr2 = newValue;
                return interpreter.Return (oldValue);
            }
            else

                throw new NotImplementedException ();
        }

        [SchemePrimitive ("SYSTEM-LIST-TO-VECTOR", 2)]
        public static object SystemListToVector (Interpreter interpreter, object arg0, object arg1)
        {
            switch ((TC) arg0)
            {
                case TC.COMBINATION:
                    return interpreter.Return(new Combination((Cons)arg1));
                case TC.ENVIRONMENT:
                    Cons list = (Cons) arg1;
                    Closure closure = (Closure) list.Car;
                    Cons tail = (Cons) list.Cdr;
                    object [] vector = (tail == null) ? new object [0] : tail.ToVector ();
                    return interpreter.Return (new InterpreterEnvironment (closure, vector));
                default:
                    throw new NotImplementedException ();
            }
        }

        [SchemePrimitive ("SYSTEM-PAIR-CAR", 1)]
        public static object SystemPairCar (Interpreter interpreter, object arg)
        {
            ISystemPair systemPair = arg as ISystemPair;
            if (systemPair == null)
            {
                if (arg is string)
                    return interpreter.Return (((string) arg).ToCharArray ());
                else
                    throw new NotImplementedException ();
            }
            return interpreter.Return (systemPair.SystemPairCar);
         }

        [SchemePrimitive ("SYSTEM-PAIR-CDR", 1)]
        public static object SystemPairCdr (Interpreter interpreter, object arg)
        {
            ISystemPair systemPair = arg as ISystemPair;
            if (systemPair == null)
                throw new NotImplementedException ();
            return
                interpreter.Return (systemPair.SystemPairCdr);
        }

        [SchemePrimitive ("SYSTEM-PAIR-SET-CAR!", 2)]
        public static object SystemPairSetCar (Interpreter interpreter, object arg, object newValue)
        {
            ISystemPair systemPair = arg as ISystemPair;
            object oldValue = systemPair.SystemPairCar;
            systemPair.SystemPairCar = newValue;
            return interpreter.Return (oldValue);
        }

        [SchemePrimitive ("SYSTEM-PAIR-SET-CDR!", 2)]
        public static object SystemPairSetCdr (Interpreter interpreter, object arg, object newValue)
        {
            ISystemPair systemPair = arg as ISystemPair;
            object oldValue = systemPair.SystemPairCdr;
            systemPair.SystemPairCdr = newValue;
            return interpreter.Return (oldValue);
        }

        [SchemePrimitive ("SYSTEM-PAIR-CONS", 3)]
        public static object SystemPairCons (Interpreter interpreter, object acode, object car, object cdr)
        {
            int icode = (int) acode;
            TC code = (TC) acode;
            switch (code)
            {
                case TC.ASSIGNMENT:
                    return interpreter.Return (new Assignment (car, cdr));
                case TC.COMBINATION_1:
                    return interpreter.Return (new Combination1 (car, cdr));
                case TC.DEFINITION:
                    return interpreter.Return (new Definition (car, cdr));
                case TC.ENTITY:
                    return interpreter.Return (new Entity (car, cdr));
                case TC.LAMBDA:
                    return interpreter.Return (new Lambda (car, cdr));
                case TC.PROCEDURE:
                    return interpreter.Return (new Closure ((Lambda) car, (cdr is bool && (bool) cdr == false) ? Environment.Global : (Environment) cdr));
                case TC.RATNUM:
                    return interpreter.Return (new Ratnum (car, cdr));
                case TC.SEQUENCE_2:
                    return interpreter.Return(new Sequence2(car, cdr));
                case TC.UNINTERNED_SYMBOL:
                    return interpreter.Return (new String ((char []) car));
                case TC.WEAK_CONS:
                    return interpreter.Return (new WeakCons (car, cdr));
                default:
                    throw new NotImplementedException ();
            }
        }

        [SchemePrimitive("SYSTEM-SUBVECTOR-TO-LIST", 3)]
        public static object SystemSubvectorToList(Interpreter interpreter, object arg, object start, object end)
        {
            ISystemVector sysVec = arg as ISystemVector;
            if (sysVec != null)
            {
                Cons answer = null;
                for (int i = (int)end - 1; i > ((int)start - 1); i--)
                {
                    answer = new Cons(sysVec.SystemVectorRef(i), answer);
                }
                return interpreter.Return(answer);
            }
            else throw new NotImplementedException();
        }

        [SchemePrimitive ("SYSTEM-VECTOR-REF", 2)]
        public static object SystemVectorRef (Interpreter interpreter, object arg, object offset)
        {
            ISystemVector sysVec = arg as ISystemVector;
            if (sysVec != null)
                return interpreter.Return (sysVec.SystemVectorRef ((int) offset));
            else
                throw new NotImplementedException ();
        }

        [SchemePrimitive ("SYSTEM-VECTOR-SIZE", 1)]
        public static object SystemVectorSize (Interpreter interpreter, object arg)
        {
            ISystemVector sysVec = arg as ISystemVector;
            if (sysVec != null)
                return interpreter.Return (sysVec.SystemVectorSize);
            else if (arg is bool [])
                return interpreter.Return ((((bool []) arg).Length / 32) + 1);
            else if (arg is int)
                return interpreter.Return (0x1000);
            else if (arg is long)
                return interpreter.Return (0x2000);
            else
                throw new NotImplementedException ();
        }
    }
}
