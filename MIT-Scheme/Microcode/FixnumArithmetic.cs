using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    static class FixnumArithmetic
    {
        static object [] SmallFixnums = { 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20 };
        [SchemePrimitive ("FIXNUM?", 1)]
        public static bool IsFixnum (out object answer, object arg0)
        {
            answer = arg0 is Int32;
            return false;
  
        }

        [SchemePrimitive ("INDEX-FIXNUM?", 1)]
        public static bool IsIndexFixnum (out object answer, object arg0)
        {
            answer = arg0 is Int32 && ((int) arg0 >= 0);
            return false;
        }

        [SchemePrimitive ("ZERO-FIXNUM?", 1)]
        public static bool ZeroP (out object answer, object arg0)
        {
            answer = arg0 is Int32 && (int) arg0 == 0;
            return false;
        }

        [SchemePrimitive ("NEGATIVE-FIXNUM?", 1)]
        public static bool NegativeP (out object answer, object arg0)
        {
            answer = (int) arg0 < 0;
            return false;
        }

        [SchemePrimitive ("POSITIVE-FIXNUM?", 1)]
        public static bool PositiveP (out object answer, object arg0)
        {
            answer = (int) arg0 > 0;
            return false;
        }

        [SchemePrimitive ("EQUAL-FIXNUM?", 2)]
        public static bool EqualP (out object answer, object left, object right)
        {
            answer = (int) left == (int) right;
            return false;
        }

        [SchemePrimitive ("LESS-THAN-FIXNUM?", 2)]
        public static bool LessP (out object answer, object left, object right)
        {
            answer = (int) left < (int) right ? Constant.sharpT : Constant.sharpF;
            return false;
        }

        [SchemePrimitive ("GREATER-THAN-FIXNUM?", 2)]
        public static bool GreaterP (out object answer, object left, object right)
        {
            answer = (int) left > (int) right;
            return false;
        }

        [SchemePrimitive ("ONE-PLUS-FIXNUM", 1)]
        public static bool Add1 (out object answer, object arg0)
        {
            answer = (int) arg0 + 1;
            return false;

        }

        [SchemePrimitive ("MINUS-ONE-PLUS-FIXNUM", 1)]
        public static bool Subtract1 (out object answer, object arg0)
        {
            int ianswer = (int) arg0 - 1;
            answer = (ianswer >= 0) && (ianswer < 16) ? SmallFixnums [ianswer] : (object) ianswer;
            return false;
        }

        [SchemePrimitive ("PLUS-FIXNUM", 2)]
        public static bool Add (out object answer, object left, object right)
        {
            answer = (int) left + (int) right;
            return false;
        }

        [SchemePrimitive ("MINUS-FIXNUM", 2)]
        public static bool Subtract (out object answer, object left, object right)
        {
            answer = (int) left - (int) right;
            return false;
        }

    //    [SchemePrimitive ("FIXNUM-NEGATE", 1)]
    //    public static PartialResult Negate (object arg0)
    //    {
    //        throw new NotImplementedException ();
    //    }

        [SchemePrimitive ("MULTIPLY-FIXNUM", 2)]
        public static bool Multiply (out object answer, object left, object right)
        {
            answer = (int) left * (int) right;
            return false;
        }

        [SchemePrimitive ("DIVIDE-FIXNUM", 2)]
        public static bool Divide (out object answer, object left, object right)
        {
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("FIXNUM-QUOTIENT", 2)]
        public static bool Quotient (out object answer, object left, object right)
        {
            answer = (int) left / (int) right;
            return false;
        }

        [SchemePrimitive ("FIXNUM-REMAINDER", 2)]
        public static bool Remainder (out object answer, object left, object right)
        {
            answer = (int) left % (int) right;
            return false;
        }

    //    [SchemePrimitive ("GCD-FIXNUM", 2)]
    //    public static PartialResult GreatestCommonDivisor (object left, object right)
    //    {
    //        throw new NotImplementedException ();
    //    }

        [SchemePrimitive ("FIXNUM-ANDC", 2)]
        public static bool AndC (out object answer, object left, object right)
        {
            answer = (int) left & ~((int) right);
            return false;
        }

        [SchemePrimitive ("FIXNUM-AND", 2)]
        public static bool And (out object answer, object left, object right)
        {
            answer = (int) left & (int) right;
            return false;
        }

        [SchemePrimitive ("FIXNUM-OR", 2)]
        public static bool Or (out object answer, object left, object right)
        {
            answer = (int) left | (int) right;
            return false;
        }

        [SchemePrimitive ("FIXNUM-XOR", 2)]
        public static bool Xor (out object answer, object left, object right)
        {
            answer = (int) left ^ (int) right;
            return false;
        }

        [SchemePrimitive ("FIXNUM-NOT", 1)]
        public static bool Not (out object answer, object arg)
        {
            answer = ~((int) arg);
            return false;
        }

        [SchemePrimitive ("FIXNUM-LSH", 2)]
        public static bool Lsh (out object answer, object left, object right)
        {
            answer = (int) left << (int) right;
            return false;
        }

        [SchemePrimitive ("FIXNUM->FLONUM", 1)]
        public static bool ToFlonum (out object answer, object arg)
        {
            answer = (double) (int) arg;
            return false;
        }
    }
}
