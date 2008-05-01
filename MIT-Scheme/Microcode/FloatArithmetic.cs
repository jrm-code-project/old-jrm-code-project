using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    struct DecodedFloat
    {
        public int sign;
        public int exponent;
        public long mantissa;

        public DecodedFloat (int sign, int exponent, long mantissa)
        {
            this.sign = sign;
            this.exponent = exponent;
            this.mantissa = mantissa;
        }
    }

    static class FloatArithmetic
    {
        static object Narrow (long arg)
        {
            if ((arg > Int32.MaxValue) || (arg < Int32.MinValue))
                return arg;
            else
                return (int) arg;
        }

        public static DecodedFloat DecodeFloat (double theFloat)
        {
            int sign = 0;
            int exponent = 0;

            if (theFloat > 0.0e0)
                sign = 1;
            if (theFloat < 0.0e0)
            {
                sign = -1;
                theFloat = -theFloat;
            }

            while (theFloat >= 9007199254740992.0)
            {
                exponent += 1;
                theFloat /= 2.0e0;
            }
            while (theFloat < 4503599627370496.0)
            {
                exponent -= 1;
                theFloat *= 2.0e0;
            }

            return new DecodedFloat (sign, exponent, (long) theFloat);
        }

        public static double EncodeFloat (int sign, int exponent, long mantissa)
        {
            double theFloat = (double) mantissa;
            while (exponent < 0)
            {
                exponent += 1;
                theFloat /= 2.0e0;
            }
            while (exponent > 0)
            {
                exponent -= 1;
                theFloat *= 2.0e0;
            }
            return theFloat * (double) sign;
        }

        static double FrExp (double value, out int eptr)
        {
            double x = ((value < 0) ? (-value) : value);
            int e = 0;
            if (x >= 1)
            {
                while (true)
                {
                    if (x > 2)
                    {
                        double xr = (x / 2);
                        double r = 2;
                        int n = 1;
                        while (xr >= r)
                        {
                            /* ((xr == (x / r)) && (xr >= r) && (x >= (r * r))) */
                            xr /= r;
                            /* ((xr == (x / (r * r))) && (xr >= 1)) */
                            r *= r;
                            /* ((xr == (x / r)) && (x >= r)) */
                            n += n;
                        }
                        /* ((xr >= 1) && (xr < r)) */
                        x = xr;
                        e += n;
                    }
                    else if (x < 2)
                    {
                        x /= 2;
                        e += 1;
                        break;
                    }
                    else
                    {
                        x /= 4;
                        e += 2;
                        break;
                    }
                }
            }
            else if ((x > 0) && (x < 0.5))
            {
                while (true)
                {
                    if (x < 0.25)
                    {
                        double xr = (x * 2);
                        double r = 0.5;
                        int n = 1;
                        /* ((xr == (x / r)) && (xr < 0.5) && (x < (r / 2))) */
                        while (xr < (r / 2))
                        {
                            /* ((xr < (r / 2)) && (x < ((r * r) / 2))) */
                            xr /= r;
                            /* ((xr == (x / (r * r))) && (xr < 0.5)) */
                            r *= r;
                            /* ((xr == (x / r)) && (x < (r / 2))) */
                            n += n;
                        }
                        /* ((xr >= (r / 2)) && (xr < 0.5)) */
                        x = xr;
                        e -= n;
                    }
                    else
                    {
                        x *= 2;
                        e -= 1;
                        break;
                    }
                }
            }
            eptr = e;
            return (value < 0) ? (-x) : x;
        }

        public static double LdExp (double value, int exponent)
        {
            double x = value;
            int e = exponent;
            double r = 2;
            if (e > 0)
            {
                if (e == 1)
                    return (x * 2);
                while (true)
                {
                    if ((e % 2) != 0)
                        x *= r;
                    e /= 2;
                    if (e == 1)
                        return (x * r * r);
                    r *= r;
                }
            }
            else if (e < 0)
            {
                e = (-e);
                if (e == 1)
                    return (x / 2);
                while (true)
                {
                    if ((e % 2) != 0)
                        x /= r;
                    e /= 2;
                    if (e == 1)
                        return ((x / r) / r);
                    r *= r;
                }
            }
            else
                return (x);
        }

        [SchemePrimitive ("FLONUM?", 1)]
        public static object IsFlonum (Interpreter interpreter, object arg0)
        {
            return interpreter.Return (arg0 is double);
        }

        [SchemePrimitive ("FLONUM-ADD", 2)]
        public static object Add (Interpreter interpreter, object left, object right)
        {
            return interpreter.Return ((double) left + (double) right);
        }

        [SchemePrimitive ("FLONUM-SUBTRACT", 2)]
        public static object Subtract (Interpreter interpreter, object left, object right)
        {
            return interpreter.Return ((double) left - (double) right);
        }

        [SchemePrimitive ("FLONUM-MULTIPLY", 2)]
        public static object Multiply (Interpreter interpreter, object left, object right)
        {
            return interpreter.Return ((double) left * (double) right);
        }

        [SchemePrimitive ("FLONUM-DIVIDE", 2)]
        public static object Divide (Interpreter interpreter, object left, object right)
        {
            return interpreter.Return ((double) left / (double) right);
        }

        [SchemePrimitive ("FLONUM-NEGATE", 1)]
        public static object Negate (Interpreter interpreter, object arg0)
        {
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("FLONUM-ABS", 1)]
        public static object Abs (Interpreter interpreter, object arg0)
        {
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("FLONUM-EQUAL?", 2)]
        public static object IsEqual (Interpreter interpreter, object left, object right)
        {
            return interpreter.Return ((double) left == (double) right);
        }

        [SchemePrimitive ("FLONUM-LESS?", 2)]
        public static object IsLess (Interpreter interpreter, object left, object right)
        {
            return interpreter.Return ((double) left < (double) right);
        }

        [SchemePrimitive ("FLONUM-GREATER?", 2)]
        public static object IsGreater (Interpreter interpreter, object left, object right)
        {
            return interpreter.Return ((double) left > (double) right);
        }

        [SchemePrimitive ("FLONUM-ZERO?", 1)]
        public static object IsZero (Interpreter interpreter, object arg)
        {
            return interpreter.Return ((double) arg == 0.0);
        }

        [SchemePrimitive ("FLONUM-POSITIVE?", 1)]
        public static object IsPositive (Interpreter interpreter, object arg)
        {
            return interpreter.Return ((double) arg > 0.0);
        }

        [SchemePrimitive ("FLONUM-NEGATIVE?", 1)]
        public static object IsNegative (Interpreter interpreter, object arg)
        {
            return interpreter.Return ((double) arg < 0.0);
        }

        [SchemePrimitive ("FLONUM-EXP", 1)]
        public static object Exp (Interpreter interpreter, object arg)
        {
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("FLONUM-LOG", 1)]
        public static object Log (Interpreter interpreter, object arg)
        {
            return interpreter.Return (Math.Log ((double) arg));
        }

        [SchemePrimitive ("FLONUM-SIN", 1)]
        public static object Sin (Interpreter interpreter, object arg)
        {
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("FLONUM-COS", 1)]
        public static object Cos (Interpreter interpreter, object arg)
        {
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("FLONUM-TAN", 1)]
        public static object Tan (Interpreter interpreter, object arg)
        {
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("FLONUM-ASIN", 1)]
        public static object Asin (Interpreter interpreter, object arg)
        {
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("FLONUM-ACOS", 1)]
        public static object Acos (Interpreter interpreter, object arg)
        {
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("FLONUM-ATAN", 1)]
        public static object Atan (Interpreter interpreter, object arg)
        {
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("FLONUM-ATAN2", 2)]
        public static object Atan2 (Interpreter interpreter, object left, object right)
        {
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("FLONUM-SQRT", 1)]
        public static object Sqrt (Interpreter interpreter, object arg)
        {
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("FLONUM-EXPT", 2)]
        public static object Expt (Interpreter interpreter, object fbase, object fpower)
        {
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("FLONUM-INTEGER?", 1)]
        public static object IsFlonumInteger (Interpreter interpreter, object arg)
        {
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("FLONUM-FLOOR", 1)]
        public static object Floor (Interpreter interpreter, object arg)
        {
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("FLONUM-CEILING", 1)]
        public static object Ceiling (Interpreter interpreter, object arg)
        {
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("FLONUM-TRUNCATE", 1)]
        public static object Truncate (Interpreter interpreter, object arg)
        {
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("FLONUM-ROUND", 1)]
        public static object Round (Interpreter interpreter, object arg)
        {
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("FLONUM-TRUNCATE->EXACT", 1)]
        public static object TruncateToExact (Interpreter interpreter, object arg)
        {
            return interpreter.Return (Narrow ((long) (Math.Truncate ((double) arg))));
        }

        [SchemePrimitive ("FLONUM-FLOOR->EXACT", 1)]
        public static object FloorToExact (Interpreter interpreter, object arg)
        {
            return interpreter.Return (Narrow ((long) (Math.Floor ((double) arg))));
        }

        [SchemePrimitive ("FLONUM-CEILING->EXACT", 1)]
        public static object CeilingToExact (Interpreter interpreter, object arg)
        {
            return interpreter.Return (Narrow ((long) (Math.Ceiling ((double) arg))));
        }

        [SchemePrimitive ("FLONUM-ROUND->EXACT", 1)]
        public static object RoundToExact (Interpreter interpreter, object arg)
        {
            return interpreter.Return (Narrow ((long) (Math.Round ((double) arg))));
        }

        [SchemePrimitive ("FLONUM-NORMALIZE", 1)]
        public static object Normalize (Interpreter interpreter, object arg)
        {
            int exponent;
            double significand = FrExp ((double) arg, out exponent);
            return interpreter.Return (new Cons (significand, exponent));
        }

        [SchemePrimitive ("FLONUM-DENORMALIZE", 2)]
        public static object Denormalize (Interpreter interpreter, object left, object right)
        {
            return interpreter.Return (LdExp ((double) left, (int) right));
        }
    }
}
