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
        public static bool IsFlonum (out object answer, object arg0)
        {
            answer = arg0 is double;
            return false;
        }

        [SchemePrimitive ("FLONUM-ADD", 2)]
        public static bool Add (out object answer, object left, object right)
        {
            answer = (double) left + (double) right;
            return false;
        }

        [SchemePrimitive ("FLONUM-SUBTRACT", 2)]
        public static bool Subtract (out object answer, object left, object right)
        {
            answer = (double) left - (double) right;
            return false;
        }

        [SchemePrimitive ("FLONUM-MULTIPLY", 2)]
        public static bool Multiply (out object answer, object left, object right)
        {
            answer = (double) left * (double) right;
            return false;
        }

        [SchemePrimitive ("FLONUM-DIVIDE", 2)]
        public static bool Divide (out object answer, object left, object right)
        {
            answer = (double) left / (double) right;
            return false;
        }

        //[SchemePrimitive ("FLONUM-NEGATE", 1)]
        //public static PartialResult Negate (object arg0)
        //{
        //    throw new NotImplementedException ();
        //}

        //[SchemePrimitive ("FLONUM-ABS", 1)]
        //public static PartialResult Abs (object arg0)
        //{
        //    throw new NotImplementedException ();
        //}

        [SchemePrimitive ("FLONUM-EQUAL?", 2)]
        public static bool IsEqual (out object answer, object left, object right)
        {
            answer = (double) left == (double) right;
            return false;
        }

        [SchemePrimitive ("FLONUM-LESS?", 2)]
        public static bool IsLess (out object answer, object left, object right)
        {
            answer = (double) left < (double) right;
            return false;
        }

        [SchemePrimitive ("FLONUM-GREATER?", 2)]
        public static bool IsGreater (out object answer, object left, object right)
        {
            answer = (double) left > (double) right;
            return false;
        }

        [SchemePrimitive ("FLONUM-ZERO?", 1)]
        public static bool IsZero (out object answer, object arg)
        {
            answer = (double) arg == 0.0;
            return false;
        }

        [SchemePrimitive ("FLONUM-POSITIVE?", 1)]
        public static bool IsPositive (out object answer, object arg)
        {
            answer = (double) arg > 0.0;
            return false;
        }

        [SchemePrimitive ("FLONUM-NEGATIVE?", 1)]
        public static bool IsNegative (out object answer, object arg)
        {
            answer = (double) arg < 0.0;
            return false;
        }

        //[SchemePrimitive ("FLONUM-EXP", 1)]
        //public static PartialResult Exp (object arg)
        //{
        //    throw new NotImplementedException ();
        //}

        [SchemePrimitive ("FLONUM-LOG", 1)]
        public static bool Log (out object answer, object arg)
        {
            answer = Math.Log ((double) arg);
            return false;
        }

        [SchemePrimitive ("FLONUM-SIN", 1)]
        public static bool Sin (out object answer, object arg)
        {
            answer = Math.Sin ((double) arg);
            return false;
        }

        //[SchemePrimitive ("FLONUM-COS", 1)]
        //public static PartialResult Cos (object arg)
        //{
        //    throw new NotImplementedException ();
        //}

        //[SchemePrimitive ("FLONUM-TAN", 1)]
        //public static PartialResult Tan (object arg)
        //{
        //    throw new NotImplementedException ();
        //}

        //[SchemePrimitive ("FLONUM-ASIN", 1)]
        //public static PartialResult Asin (object arg)
        //{
        //    throw new NotImplementedException ();
        //}

        //[SchemePrimitive ("FLONUM-ACOS", 1)]
        //public static PartialResult Acos (object arg)
        //{
        //    throw new NotImplementedException ();
        //}

        //[SchemePrimitive ("FLONUM-ATAN", 1)]
        //public static PartialResult Atan (object arg)
        //{
        //    throw new NotImplementedException ();
        //}

        //[SchemePrimitive ("FLONUM-ATAN2", 2)]
        //public static PartialResult Atan2 (object left, object right)
        //{
        //    throw new NotImplementedException ();
        //}

        //[SchemePrimitive ("FLONUM-SQRT", 1)]
        //public static PartialResult Sqrt (object arg)
        //{
        //    throw new NotImplementedException ();
        //}

        //[SchemePrimitive ("FLONUM-EXPT", 2)]
        //public static PartialResult Expt (object fbase, object fpower)
        //{
        //    throw new NotImplementedException ();
        //}

        //[SchemePrimitive ("FLONUM-INTEGER?", 1)]
        //public static PartialResult IsFlonumInteger (object arg)
        //{
        //    throw new NotImplementedException ();
        //}

        [SchemePrimitive ("FLONUM-FLOOR", 1)]
        public static bool Floor (out object answer, object arg)
        {
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("FLONUM-CEILING", 1)]
        public static bool Ceiling (out object answer, object arg)
        {
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("FLONUM-TRUNCATE", 1)]
        public static bool Truncate (out object answer, object arg)
        {
            answer = Math.Truncate ((double) arg);
            return false;
        }

        [SchemePrimitive ("FLONUM-ROUND", 1)]
        public static bool Round (out object answer, object arg)
        {
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("FLONUM-TRUNCATE->EXACT", 1)]
        public static bool TruncateToExact (out object answer, object arg)
        {
            answer = Narrow ((long) (Math.Truncate ((double) arg)));
            return false;
        }

        [SchemePrimitive ("FLONUM-FLOOR->EXACT", 1)]
        public static bool FloorToExact (out object answer, object arg)
        {
            answer = Narrow ((long) (Math.Floor ((double) arg)));
            return false;
        }

        [SchemePrimitive ("FLONUM-CEILING->EXACT", 1)]
        public static bool CeilingToExact (out object answer, object arg)
        {
            answer = Narrow ((long) (Math.Ceiling ((double) arg)));
            return false;
        }

        [SchemePrimitive ("FLONUM-ROUND->EXACT", 1)]
        public static bool RoundToExact (out object answer, object arg)
        {
            answer = Narrow ((long) (Math.Round ((double) arg)));
            return false;
        }

        [SchemePrimitive ("FLONUM-NORMALIZE", 1)]
        public static bool Normalize (out object answer, object arg)
        {
            int exponent;
            double significand = FrExp ((double) arg, out exponent);
            answer = new Cons (significand, exponent);
            return false;
        }

        [SchemePrimitive ("FLONUM-DENORMALIZE", 2)]
        public static bool Denormalize (out object answer, object left, object right)
        {
            answer = LdExp ((double) left, (int) right);
            return false;
        }
    }
}
