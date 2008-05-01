using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    static class GenericArithmetic
    {
        static object Narrow (long arg)
        {
            if ((arg > Int32.MaxValue) || (arg < Int32.MinValue))
                return arg;
            else
                return (int) arg;
        }

        static long Widen (object arg)
        {
            if (arg is int)
                return (long) (int) arg;
            else if (arg is long)
                return (long) arg;
            else
                throw new NotImplementedException ();
        }

        [SchemePrimitive ("&+", 2)]
        public static object Add (Interpreter interpreter, object left, object right)
        {
            if (left is int)
            {
                if (right is int)
                    return interpreter.Return ((int) left + (int) right);
                else
                    throw new NotImplementedException ();
            }
            else if (left is long)
            {
                if (right is long)
                    return interpreter.Return (Narrow ((long) left + (long) right));
                else
                    throw new NotImplementedException ();
            }
            else
                return interpreter.Apply (FixedObjectsVector.GenericAdd, new object [] { left, right });
        }

        [SchemePrimitive ("&-", 2)]
        public static object Subtract (Interpreter interpreter, object left, object right)
        {
            if (left is int)
            {
                if (right is int)
                    return interpreter.Return (Narrow (Widen (left) - Widen (right)));
                else if (right is long)
                    return interpreter.Return (Narrow (Widen (left) - Widen (right)));
                else
                    throw new NotImplementedException ();
            }
            else if (left is long)
            {
                if (right is int)
                    return interpreter.Return (Narrow (Widen (left) - Widen (right)));
                else if (right is long)
                    return interpreter.Return (Narrow (Widen (left) - Widen (right)));
                else
                    throw new NotImplementedException ();
            }
            else
                return interpreter.Apply (FixedObjectsVector.GenericSubtract, new object [] { left, right });
        }

        [SchemePrimitive ("&*", 2)]
        public static object Multiply (Interpreter interpreter, object left, object right)
        {
            if (left is int)
            {
                if (right is int)
                    return interpreter.Return (Narrow (Widen (left) * Widen (right)));
                else if (right is long)
                    return interpreter.Return (Narrow (Widen (left) * (long) right));
                else if (right is double)
                    return interpreter.Return ((double) (int) left * (double) right);
                else
                    throw new NotImplementedException ();
            }
            else if (left is long)
            {
                if (right is int)
                    return interpreter.Return (Bignum.ToInteger ((Bignum) (long) left * (Bignum) (int) right));
                else
                    throw new NotImplementedException ();
            }
            else
                return interpreter.Apply (FixedObjectsVector.GenericMultiply, new object [] { left, right });
        }

        [SchemePrimitive ("&/", 2)]
        public static object Divide (Interpreter interpreter, object left, object right)
        {
            return interpreter.Apply (FixedObjectsVector.GenericDivide, new object [] { left, right });
        }

        [SchemePrimitive ("QUOTIENT", 2)]
        public static object Quotient (Interpreter interpreter, object left, object right)
        {
            return interpreter.Apply (FixedObjectsVector.GenericQuotient, new object [] { left, right });
        }

        [SchemePrimitive ("REMAINDER", 2)]
        public static object Remainder (Interpreter interpreter, object left, object right)
        {
            return interpreter.Apply (FixedObjectsVector.GenericRemainder, new object [] { left, right });
        }

        [SchemePrimitive ("MODULO", 2)]
        public static object Modulo (Interpreter interpreter, object left, object right)
        {
            return interpreter.Apply (FixedObjectsVector.GenericModulo, new object [] { left, right });
        }

        [SchemePrimitive ("&=", 2)]
        public static object IsEqual (Interpreter interpreter, object left, object right)
        {
            if (left is int)
            {
                if (right is int)
                    return interpreter.Return ((int) left == (int) right);
                else
                    throw new NotImplementedException ();
            }
            else if (left is long)
            {
                if (right is long)
                    return interpreter.Return ((long) left == (long) right);
                else
                    throw new NotImplementedException ();
            }
            else if (left is Bignum)
            {
                if (right is Bignum)
                    return interpreter.Return ((Bignum) left == (Bignum) right);
                else
                    throw new NotImplementedException ();
            }
            else

                return interpreter.Apply (FixedObjectsVector.GenericEqualP, new object [] { left, right });
        }

        [SchemePrimitive ("&<", 2)]
        public static object IsLess (Interpreter interpreter, object left, object right)
        {
            if (left is int)
            {
                if (right is int)
                    return interpreter.Return ((int) left < (int) right);
                else
                    throw new NotImplementedException ();
            }
            else if (left is long)
            {
                if (right is int)
                    return interpreter.Return ((long) left < (int) right);
                else
                    throw new NotImplementedException ();
            }
            else
                return interpreter.Apply (FixedObjectsVector.GenericLessP, new object [] { left, right });
        }

        [SchemePrimitive ("&>", 2)]
        public static object IsGreater (Interpreter interpreter, object left, object right)
        {
            if (left is int)
            {
                if (right is int)
                    return interpreter.Return ((int) left > (int) right);
                else
                    throw new NotImplementedException ();
            }
            else
                return interpreter.Apply (FixedObjectsVector.GenericGreaterP, new object [] { left, right });
        }

        [SchemePrimitive ("ZERO?", 1)]
        public static object IsZero (Interpreter interpreter, object arg)
        {
            if (arg is int)
                return interpreter.Return ((int) arg == 0);
            else
                return interpreter.Apply (FixedObjectsVector.GenericZeroP, new object [] { arg });
        }

        [SchemePrimitive ("POSITIVE?", 1)]
        public static object IsPositive (Interpreter interpreter, object arg)
        {
            if (arg is int)
                return interpreter.Return ((int) arg > 0);
            else
                return interpreter.Apply (FixedObjectsVector.GenericPositiveP, new object [] { arg });
        }

        [SchemePrimitive ("NEGATIVE?", 1)]
        public static object IsNegative (Interpreter interpreter, object arg)
        {
            if (arg is int)
                return interpreter.Return ((int) arg < 0);
            else
                return interpreter.Apply (FixedObjectsVector.GenericNegativeP, new object [] { arg });
        }

        [SchemePrimitive ("1+", 1)]
        public static object Increment (Interpreter interpreter, object arg)
        {
            if (arg is int)
                return interpreter.Return ((int) arg + 1);
            else
                return interpreter.Apply (FixedObjectsVector.GenericAdd, new object [2] { arg, 1 });
        }

        [SchemePrimitive ("-1+", 1)]
        public static object Decrement (Interpreter interpreter, object arg)
        {
            if (arg is int)
                return interpreter.Return ((int) arg - 1);
            else if (arg is long)
                return interpreter.Return (Narrow ((long) arg - 1));
            else
                return interpreter.Apply (FixedObjectsVector.GenericSubtract, new object [2] { arg, 1 });
        }
    }
}
