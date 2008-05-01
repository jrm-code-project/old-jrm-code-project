using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    static class IntegerArithmetic
    {
        static object Narrow (long arg)
        {
            if ((arg > Int32.MaxValue) || (arg < Int32.MinValue))
                return arg;
            else
                return (int) arg;
        }

        static object Narrow (Decimal arg)
        {
            if ((arg > Int32.MaxValue) || (arg < Int32.MinValue))
            {
                if ((arg > Int64.MaxValue) || (arg < Int64.MinValue))
                    return arg;
                else
                    return (long) arg;
            }
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

        [SchemePrimitive ("INTEGER-ZERO?", 1)]
        public static object ZeroP (Interpreter interpreter, object arg)
        {
            if (arg is int)
                return interpreter.Return ((int) (arg) == 0);
            else if (arg is long)
                return interpreter.Return ((long) (arg) == 0);
            else
                throw new NotImplementedException ();
        }

        [SchemePrimitive ("INTEGER-NEGATIVE?", 1)]
        public static object NegativeP (Interpreter interpreter, object arg)
        {
            if (arg is int)
                return interpreter.Return ((int) (arg) < 0);
            else if (arg is long)
                return interpreter.Return ((long) (arg) < 0);
            else
                throw new NotImplementedException ();
        }

        [SchemePrimitive ("INTEGER-POSITIVE?", 1)]
        public static object PositiveP (Interpreter interpreter, object arg)
        {
            if (arg is int)
                return interpreter.Return ((int) (arg) > 0);
            else if (arg is long)
                return interpreter.Return ((long) (arg) > 0);
            else
                throw new NotImplementedException ();
        }

        [SchemePrimitive ("INTEGER-EQUAL?", 2)]
        public static object EqualP (Interpreter interpreter, object left, object right)
        {
            if (left is int)
            {
                if (right is int)
                    return interpreter.Return ((int) left == (int) (right));
                else if (right is long)
                    return interpreter.Return ((int) left == (long) (right));
                else
                    throw new NotImplementedException ();
            }
            else if (left is long)
            {
                if (right is int)
                    throw new NotImplementedException ();
                else if (right is long)
                    return interpreter.Return ((long) left == (long) (right));
                else
                    throw new NotImplementedException ();
            }
            else if (left is Bignum)
            {
                if (right is Bignum)
                    return interpreter.Return ((Bignum) left == (Bignum) (right));
                else
                    throw new NotImplementedException ();
            }
            // kludge
            else if (left is History && right is History)
                return interpreter.Return (((History) left).Element == ((History) (right)).Element);
            else
                throw new NotImplementedException ();
        }

        [SchemePrimitive ("INTEGER-LESS?", 2)]
        public static object LessP (Interpreter interpreter, object left, object right)
        {
            if (left is int)
            {
                if (right is int)
                    return interpreter.Return ((int) left < (int) (right));
                throw new NotImplementedException ();
            }
            else if (left is long)
            {
                if (right is int)
                    return interpreter.Return ((long) left < (int) right);
                else if (right is long)
                    return interpreter.Return ((long) left < (long) right);
                else
                    throw new NotImplementedException ();
            }
            else
                throw new NotImplementedException ();
        }

        [SchemePrimitive ("INTEGER-GREATER?", 2)]
        public static object GreaterP (Interpreter interpreter, object left, object right)
        {
            if (left is int)
            {
                if (right is int)
                    return interpreter.Return ((int) left > (int) right);
                else if (right is long)
                    return interpreter.Return ((long) (int) left > (long) right);
                else
                    throw new NotImplementedException ();
            }
            else if (left is long)
            {
                if (right is long)
                    return interpreter.Return ((long) left > (long) right);
                else
                    throw new NotImplementedException ();
            }
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("INTEGER-ADD", 2)]
        public static object Add (Interpreter interpreter, object left, object right)
        {
            if (left is int)
            {
                if (right is int)
                    return interpreter.Return (Narrow ((long) (int) left + (long) (int) (right)));
                throw new NotImplementedException ();
            }
            else if (left is long)
            {
                if (right is int)
                    return interpreter.Return (Narrow ((long) left + (long) (int) (right)));
                else if (right is long)
                    return interpreter.Return (Narrow ((long) left + (long) (right)));
                else
                    throw new NotImplementedException ();
            }
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("INTEGER-SUBTRACT", 2)]
        public static object Subtract (Interpreter interpreter, object left, object right)
        {
            if (left is int)
            {
                if (right is int)
                    return interpreter.Return (Narrow ((long) (int) left - (long) (int) right));
                else if (right is long)
                    return interpreter.Return (Narrow ((long) (int) left - (long) right));
                else
                    throw new NotImplementedException ();
            }
            else if (left is long)
            {
                if (right is int)
                    return interpreter.Return (Narrow ((long) left - (long) (int) right));
                else if (right is long)
                    return interpreter.Return (Narrow ((long) left - (long) right));
                else
                    throw new NotImplementedException ();
            }

            throw new NotImplementedException ();
        }

        [SchemePrimitive ("INTEGER-MULTIPLY", 2)]
        public static object Multiply (Interpreter interpreter, object left, object right)
        {
            if (left is int)
            {
                if (right is int)
                    return interpreter.Return (Narrow (Widen (left) * Widen (right)));
                else if (right is long)
                    return interpreter.Return (Bignum.ToInteger ((Bignum) (long) (int) left * (Bignum) (long) (right)));
                else if (right is Bignum)
                    return interpreter.Return (Bignum.ToInteger ((Bignum) (long) (int) left * (Bignum) (right)));
                else
                    throw new NotImplementedException ();
            }
            else if (left is long)
            {
                if (right is int)
                    return interpreter.Return (Bignum.ToInteger ((Bignum) (long) (left) * (Bignum) (long) (int) (right)));
                else if (right is long)
                    return interpreter.Return (Bignum.ToInteger ((Bignum) (long) (left) * (Bignum) (long) (right)));
                else if (right is Bignum)
                    return interpreter.Return (Bignum.ToInteger ((Bignum) (long) (left) * (Bignum) (right)));
                else
                    throw new NotImplementedException ();
            }
            else if (left is Bignum)
            {
                if (right is int)
                    throw new NotImplementedException ();
                else if (right is Bignum)
                    return interpreter.Return (Bignum.ToInteger ((Bignum) (left) * (Bignum) (right)));
                else
                    throw new NotImplementedException ();
            }
            else
                throw new NotImplementedException ();
        }

        [SchemePrimitive ("INTEGER-NEGATE", 1)]
        public static object Negate (Interpreter interpreter, object arg)
        {
            if (arg is int)
                return interpreter.Return (-((int) (arg)));
            else
                throw new NotImplementedException ();
        }

        [SchemePrimitive ("INTEGER-ADD-1", 1)]
        public static object Add1 (Interpreter interpreter, object arg)
        {
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("INTEGER-SUBTRACT-1", 1)]
        public static object Subtract1 (Interpreter interpreter, object arg)
        {
            if (arg is int)
                return interpreter.Return (Narrow (((long) (int) (arg)) - 1));
            throw new NotImplementedException ();
        }

        public static object LengthInBits (Interpreter interpreter, object arg)
        {
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("INTEGER-DIVIDE", 2)]
        public static object Divide (Interpreter interpreter, object left, object right)
        {
            long w0 = Widen (left);
            long w1 = Widen (right);
            long remainder = 0;
            long quotient = Math.DivRem (w0, w1, out remainder);

            return interpreter.Return (new Cons (Narrow (quotient), Narrow (remainder)));
        }

        [SchemePrimitive ("INTEGER-QUOTIENT", 2)]
        public static object Quotient (Interpreter interpreter, object left, object right)
        {
            if (left is int)
            {
                if (right is int)
                    return interpreter.Return ((int) (left) / (int) (right));
                else
                    throw new NotImplementedException ();
            }
            else if (left is long)
            {
                if (right is int)
                    return interpreter.Return (Narrow ((long) (left) / (long) (int) (right)));
                else if (right is long)
                    return interpreter.Return (Narrow ((long) (left) / (long) (right)));
                else
                    throw new NotImplementedException ();

            }
            else if (left is Bignum)
            {
                if (right is int)
                    return interpreter.Return (Bignum.ToInteger (Bignum.Quotient ((Bignum) (left), (Bignum) (long) (int) (right))));
                else
                    throw new NotImplementedException ();
            }
            else
                throw new NotImplementedException ();
        }

        [SchemePrimitive ("INTEGER-REMAINDER", 2)]
        public static object Remainder (Interpreter interpreter, object left, object right)
        {
            if (left is int)
            {
                if (right is int)
                {
                    long remainder = 0;
                    long quotient = Math.DivRem ((long) (int) (left), (long) (int) (right), out remainder);
                    return interpreter.Return (Narrow (remainder));
                }
                else if (right is long)
                {
                    long remainder = 0;
                    long quotient = Math.DivRem ((long) (int) (left), (long) (right), out remainder);
                    return interpreter.Return (Narrow (remainder));
                }
                else
                {
                    throw new NotImplementedException ();
                }
            }
            else if (left is long)
            {
                if (right is int)
                {
                    long remainder = 0;
                    long quotient = Math.DivRem ((long) (left), (long) (int) (right), out remainder);
                    return interpreter.Return (Narrow (remainder));
                }
                else if (right is long)
                {
                    long remainder = 0;
                    long quotient = Math.DivRem ((long) (left), (long) (right), out remainder);
                    return interpreter.Return (Narrow (remainder));
                }
                else
                    throw new NotImplementedException ();
            }
            else
                throw new NotImplementedException ();
        }

        [SchemePrimitive ("INTEGER?", 1)]
        public static object IsInteger (Interpreter interpreter, object arg)
        {
            if (arg is int)
                return interpreter.Return (true);
            else
                throw new NotImplementedException ();
        }

        [SchemePrimitive ("INTEGER->FLONUM", 2)]
        public static object ToFlonum (Interpreter interpreter, object left, object right)
        {
            if (left is int)
                return interpreter.Return ((double) (int) (left));

            else if (left is long)
                return interpreter.Return ((double) (long) (left));
            else if (left is Bignum)
                return interpreter.Return (((Bignum) (left)).ToDouble ());
            else
                throw new NotImplementedException ();
        }

        [SchemePrimitive ("INTEGER-SHIFT-LEFT", 2)]
        public static object ShiftLeft (Interpreter interpreter, object left, object right)
        {
            return interpreter.Return (Narrow (Widen (left) << (int) (right)));
        }
    }
}
