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

        [SchemePrimitive ("&+", 2, false)]
        public static bool Add (out object answer, object left, object right)
        {
            if (left is int && right is int) {
                answer = (int) left + (int) right;
                return false;
            }
            answer = new TailCallInterpreter (CallFromPrimitive.Make ((IApplicable) FixedObjectsVector.GenericAdd, left, right), null);
            return true;
        }

        [SchemePrimitive ("&-", 2, false)]
        public static bool Subtract (out object answer, object left, object right)
        {
            answer = new TailCallInterpreter (CallFromPrimitive.Make ((IApplicable) FixedObjectsVector.GenericSubtract, left, right), null);
            return true;
        }

        [SchemePrimitive ("&*", 2, false)]
        public static bool Multiply (out object answer, object left, object right)
        {
            answer = new TailCallInterpreter (CallFromPrimitive.Make ((IApplicable) FixedObjectsVector.GenericMultiply, left, right), null);
            return true;
        }

        [SchemePrimitive ("&/", 2, false)]
        public static bool Divide (out object answer, object left, object right)
        {
            answer = new TailCallInterpreter (CallFromPrimitive.Make ((IApplicable) FixedObjectsVector.GenericDivide, left, right), null);
            return true;
        }

        [SchemePrimitive ("QUOTIENT", 2, false)]
        public static bool Quotient (out object answer, object left, object right)
        {
            answer = new TailCallInterpreter (CallFromPrimitive.Make ((IApplicable) FixedObjectsVector.GenericQuotient, left, right), null);
            return true;
        }

        [SchemePrimitive ("REMAINDER", 2, false)]
        public static bool Remainder (out object answer, object left, object right)
        {
            answer = new TailCallInterpreter (CallFromPrimitive.Make ((IApplicable) FixedObjectsVector.GenericRemainder, left, right), null);
            return true;
        }

        [SchemePrimitive ("MODULO", 2, false)]
        public static bool Modulo (out object answer, object left, object right)
        {
            throw new NotImplementedException ();
            //return interpreter.Apply (FixedObjectsVector.GenericModulo, new object [] { left, right });
        }

        [SchemePrimitive ("&=", 2, false)]
        public static bool IsEqual (out object answer, object left, object right)
        {
            if (left is int) {
                if (right is int)
                    answer = (int) left == (int) right;
                else if (right is long)
                    answer = (long) (int) left == (long) right;
                else
                    throw new NotImplementedException ();
            }
            else if (left is long) {
                if (right is long)
                    answer = (long) left == (long) right;
                else
                    throw new NotImplementedException ();
            }
            else if (left is Bignum) {
                if (right is Bignum)
                    answer = (Bignum) left == (Bignum) right;
                else
                    throw new NotImplementedException ();
            }
            else
                throw new NotImplementedException ();
            return false;
            //return interpreter.Apply (FixedObjectsVector.GenericEqualP, new object [] { left, right });
        }

        [SchemePrimitive ("&<", 2, false)]
        public static bool IsLess (out object answer, object left, object right)
        {
            // Shortcut for fixnums
            if (left is int && right is int) {
                answer = ((int) left < (int) right) ? Constant.sharpT : Constant.sharpF;
                return false;
            }
            else
                answer = new TailCallInterpreter (CallFromPrimitive.Make ((IApplicable) FixedObjectsVector.GenericLessP, left, right), null);
                return true;
        }

        [SchemePrimitive ("&>", 2, false)]
        public static bool IsGreater (out object answer, object left, object right)
        {
                answer = new TailCallInterpreter (CallFromPrimitive.Make ((IApplicable) FixedObjectsVector.GenericGreaterP, left, right), null);
                return true;
        }

        [SchemePrimitive ("ZERO?", 1, false)]
        public static bool IsZero (out object answer, object arg)
        {
            answer = new TailCallInterpreter (CallFromPrimitive.Make ((IApplicable) FixedObjectsVector.GenericZeroP, arg), null);
            return true;
        }

        [SchemePrimitive ("POSITIVE?", 1, false)]
        public static bool IsPositive (out object answer, object arg)
        {
            answer = new TailCallInterpreter (CallFromPrimitive.Make ((IApplicable) FixedObjectsVector.GenericPositiveP, arg), null);
            return true;
        }

        [SchemePrimitive ("NEGATIVE?", 1, false)]
        public static bool IsNegative (out object answer, object arg)
        {
            if (arg is int)
                answer = (int) arg < 0;
            else
                throw new NotImplementedException ();
            //return interpreter.Apply (FixedObjectsVector.GenericNegativeP, new object [] { arg });
            return false;
        }

        [SchemePrimitive ("1+", 1, false)]
        public static bool Increment (out object answer, object arg)
        {
            if (arg is int)
                answer = (int) arg + 1;
            else
                throw new NotImplementedException ();
            // return interpreter.Apply (FixedObjectsVector.GenericAdd, new object [2] { arg, 1 });
            return false;
        }

        [SchemePrimitive ("-1+", 1, false)]
        public static bool Decrement (out object answer, object arg)
        {
            if (arg is int)
                answer = (int) arg - 1;
            else if (arg is long)
                answer = Narrow ((long) arg - 1);
            else
                throw new NotImplementedException ();
            // return interpreter.Apply (FixedObjectsVector.GenericSubtract, new object [2] { arg, 1 });
            return false;
        }
    }
}
