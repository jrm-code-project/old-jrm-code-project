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

    sealed class ObjectModel
    {
        private ObjectModel ()
        {
        }

        [SchemePrimitive ("EQ?", 2)]
        public static object Eq (Interpreter interpreter, object arg0, object arg1)
        {
            if (Object.ReferenceEquals (arg0, arg1))
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

        [SchemePrimitive ("NULL?", 1)]
        public static object IsNull (Interpreter interpreter, object arg)
        {
            return interpreter.Return (arg == null);
            // return interpreter.Return (arg == null || (arg is bool && (bool) (arg) == false));
        }

        [SchemePrimitive ("OBJECT-DATUM", 1)]
        public static object ObjectDatum (Interpreter interpreter, object arg)
        {
            if (arg is bool && (bool) arg == false)
                return interpreter.Return (0);
            if (arg == null)
                return interpreter.Return (0);
            return interpreter.Return (arg);
        }

        [SchemePrimitive ("OBJECT-TYPE?", 2)]
        public static object IsObjectType (Interpreter interpreter, object arg0, object arg1)
        {
            TC targetType = (TC) arg0;
            switch (targetType)
            {
                case TC.BIG_FIXNUM:
                    return interpreter.Return (arg1 is Int64);
                case TC.BIG_FLONUM:
                    return interpreter.Return (arg1 is double);
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
                //case TC.COMPLEX:
                //    return interpreter.Return (arg1 is Complex);
                //case TC.DELAYED:
                //    return interpreter.Return (arg1 is Promise);
                //case TC.ENTITY:
                //    return interpreter.Return (arg1 is Entity);
                case TC.ENVIRONMENT:
                    return interpreter.Return (arg1 is Environment);
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
                    return interpreter.Return (arg1 is Lambda);
                case TC.LIST:
                    return interpreter.Return (arg1 is Cons);
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
                //case TC.RATNUM:
                //    return interpreter.Return (arg1 is Ratnum);
                //case TC.RETURN_CODE:
                //    return interpreter.Return (arg1 is ReturnCode);
                case TC.SEQUENCE_2:
                    return interpreter.Return (arg1 is Sequence2);
                case TC.SEQUENCE_3:
                    return interpreter.Return (arg1 is Sequence3);
                case TC.UNINTERNED_SYMBOL:
                    return interpreter.Return ((arg1 is string)
                    && (Misc.IsGensym ((string) arg1)));
                case TC.VECTOR:
                    return interpreter.Return (arg1 is object []);
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
                return interpreter.Return (new TopLevelEnvironment ());
            switch (newType)
            {
                case TC.CONSTANT:
                    switch ((int) arg1)
                    {
                        case 1:
                            return interpreter.Return (Constant.Unspecific);
                        case 3:
                            return interpreter.Return (Constant.LambdaOptionalTag);
                        case 4:
                            return interpreter.Return (Constant.LambdaRestTag);
                        default:
                            throw new NotImplementedException ();
                    }
                case TC.ENVIRONMENT:
                    return interpreter.Return (new InterpreterEnvironment ((object []) arg1));
                case TC.RECORD:
                    return interpreter.Return (new Record ((object []) arg1));
                default:
                    throw new NotImplementedException ();
            }
        }

        [SchemePrimitive ("PRIMITIVE-OBJECT-SET-TYPE", 2)]
        public static object PrimitiveObjectSetType (Interpreter interpreter, object arg0, object arg1)
        {
            TC newType = (TC) (int) arg0;
            switch (newType)
            {
                case TC.FIXNUM:
                    return interpreter.Return (arg1.GetHashCode ());
                case TC.MANIFEST_SPECIAL_NM_VECTOR:
                    return interpreter.Return (new ManifestSpecialNMVector ((int) arg1));
                case TC.NON_MARKED_VECTOR:
                    return interpreter.Return (new NonMarkedVector (arg1));
                case TC.REFERENCE_TRAP:
                    if (arg1 is int && (int) arg1 == 0)
                        return interpreter.Return (Constant.Unassigned);
                    else
                        return interpreter.Return (ReferenceTrap.Make (arg1));
                default:
                    throw new NotImplementedException ();
            }
        }

        [SchemePrimitive ("SYSTEM-LIST-TO-VECTOR", 2)]
        public static object SystemListToVector (Interpreter interpreter, object arg0, object arg1)
        {
            switch ((TC) arg0)
            {
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

        [SchemePrimitive ("SYSTEM-PAIR-CONS", 3)]
        public static object SystemPairCons (Interpreter interpreter, object acode, object car, object cdr)
        {
            int icode = (int) acode;
            TC code = (TC) acode;
            switch (code)
            {
                //case TC.ENTITY:
                //    return interpreter.Return (new Entity (car, cdr));
                case TC.LAMBDA:
                    return interpreter.Return (new Lambda (car, cdr));
                case TC.PROCEDURE:
                    return interpreter.Return (new Closure ((Lambda) car, cdr == null ? Environment.Global : (Environment) cdr));
                //case TC.RATNUM:
                //    return interpreter.Return (new Ratnum (car, cdr));
                case TC.UNINTERNED_SYMBOL:
                    return interpreter.Return (new String ((char []) car));
                //case TC.WEAK_CONS:
                //    return interpreter.Return (new WeakCons (car, cdr));
                default:
                    throw new NotImplementedException ();
            }
        }
    }
}
