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
        public static bool Add (out object answer, object left, object right)
        {
            if (left is int) {
                if (right is int)
                    answer = (int) left + (int) right;
                else
                    throw new NotImplementedException ();
            }
            else if (left is long) {
                if (right is long)
                    answer = Narrow ((long) left + (long) right);
                else
                    throw new NotImplementedException ();
            }
            else {
                answer = new TailCallInterpreter (new ApplyFromPrimitive ((IApplicable) FixedObjectsVector.GenericAdd, new Cons (FixedObjectsVector.GenericAdd, new Cons (left, new Cons (right, null)))), null);
                return true;
            }
            // return interpreter.Apply (FixedObjectsVector.GenericAdd, new object [] { left, right });
            return false;
        }

        [SchemePrimitive ("&-", 2)]
        public static bool Subtract (out object answer, object left, object right)
        {
            if (left is int) {
                if (right is int)
                    answer = Narrow (Widen (left) - Widen (right));
                else if (right is long)
                    answer = Narrow (Widen (left) - Widen (right));
                else if (right is double)
                    answer = (double) (int) (left) - (double) (right);
                else
                    throw new NotImplementedException ();
            }
            else if (left is long) {
                if (right is int)
                    answer = Narrow (Widen (left) - Widen (right));
                else if (right is long)
                    answer = Narrow (Widen (left) - Widen (right));
                else
                    throw new NotImplementedException ();
            }
            else {
                answer = new TailCallInterpreter (new ApplyFromPrimitive ((IApplicable) FixedObjectsVector.GenericSubtract, new Cons (FixedObjectsVector.GenericSubtract, new Cons (left, new Cons (right, null)))), null);
                return true;
            }
            return false;
        }

        [SchemePrimitive ("&*", 2)]
        public static bool Multiply (out object answer, object left, object right)
        {
            if (left is int) {
                if (right is int)
                    answer = Narrow (Widen (left) * Widen (right));
                else if (right is long)
                    answer = Narrow (Widen (left) * (long) right);
                else if (right is double)
                    answer = (double) (int) left * (double) right;
                else
                    throw new NotImplementedException ();
            }
            else if (left is long) {
                if (right is int)
                    answer = Bignum.ToInteger ((Bignum) (long) left * (Bignum) (int) right);
                else
                    throw new NotImplementedException ();
            }
            else {
                answer = new TailCallInterpreter (new ApplyFromPrimitive ((IApplicable) FixedObjectsVector.GenericMultiply, new Cons (FixedObjectsVector.GenericMultiply, new Cons (left, new Cons (right, null)))), null);
                return true;
            }
            return false;
            //return interpreter.Apply (FixedObjectsVector.GenericMultiply, new object [] { left, right });
        }

        [SchemePrimitive ("&/", 2)]
        public static bool Divide (out object answer, object left, object right)
        {
            answer = new TailCallInterpreter (new ApplyFromPrimitive ((IApplicable) FixedObjectsVector.GenericDivide, new Cons (FixedObjectsVector.GenericDivide, new Cons (left, new Cons (right, null)))), null);
            return true;
        }

        [SchemePrimitive ("QUOTIENT", 2)]
        public static bool Quotient (out object answer, object left, object right)
        {
            answer = new TailCallInterpreter (new ApplyFromPrimitive ((IApplicable) FixedObjectsVector.GenericQuotient, new Cons (FixedObjectsVector.GenericQuotient, new Cons (left, new Cons (right, null)))), null);
            return true;
            //throw new NotImplementedException ();
            //answer = new TailCallInterpreter ((SCode) , new InterpreterEnvironment (new object [] { FixedObjectsVector.GenericQuotient, left, right }));
            //return true;

            //return Interpreter.Apply (FixedObjectsVector.GenericQuotient, new object [] { left, right });
        }

        [SchemePrimitive ("REMAINDER", 2)]
        public static bool Remainder (out object answer, object left, object right)
        {
            answer = new TailCallInterpreter (new ApplyFromPrimitive ((IApplicable) FixedObjectsVector.GenericRemainder, new Cons (FixedObjectsVector.GenericRemainder, new Cons (left, new Cons (right, null)))), null);
            return true;
        }

        [SchemePrimitive ("MODULO", 2)]
        public static bool Modulo (out object answer, object left, object right)
        {
            throw new NotImplementedException ();
            //return interpreter.Apply (FixedObjectsVector.GenericModulo, new object [] { left, right });
        }

        [SchemePrimitive ("&=", 2)]
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

        [SchemePrimitive ("&<", 2)]
        public static bool IsLess (out object answer, object left, object right)
        {
            if (left is int) {
                if (right is int)
                    answer = (int) left < (int) right;
                else
                    throw new NotImplementedException ();
            }
            else if (left is long) {
                if (right is long)
                    answer = (long) left < (long) right;
                else if (right is int)
                    answer = (long) left < (int) right;
                else
                    throw new NotImplementedException ();
            }
            else
                throw new NotImplementedException ();
            return false;
            //return interpreter.Apply (FixedObjectsVector.GenericLessP, new object [] { left, right });
        }

        [SchemePrimitive ("&>", 2)]
        public static bool IsGreater (out object answer, object left, object right)
        {
            if (left is int) {
                if (right is int) {
                    answer = (int) left > (int) right;
                    return false;
                }
                else
                    throw new NotImplementedException ();
            }
            else
                throw new NotImplementedException ();
            // return interpreter.Apply (FixedObjectsVector.GenericGreaterP, new object [] { left, right });
        }

        [SchemePrimitive ("ZERO?", 1)]
        public static bool IsZero (out object answer, object arg)
        {
            if (arg is int) {
                answer = (int) arg == 0;
                return false;
            }
            else {
                answer = new TailCallInterpreter (new ApplyFromPrimitive ((IApplicable) FixedObjectsVector.GenericZeroP, new Cons (FixedObjectsVector.GenericZeroP, new Cons (arg,  null))), null);
            return true;
            }
        }

        [SchemePrimitive ("POSITIVE?", 1)]
        public static bool IsPositive (out object answer, object arg)
        {
            if (arg is int) {
                answer = (int) arg > 0;
                return false;
            }
            else {
                 answer = new TailCallInterpreter (new ApplyFromPrimitive ((IApplicable) FixedObjectsVector.GenericPositiveP, new Cons (FixedObjectsVector.GenericPositiveP, new Cons (arg, null))), null);
            return true;
            }
        }

        [SchemePrimitive ("NEGATIVE?", 1)]
        public static bool IsNegative (out object answer, object arg)
        {
            if (arg is int)
                answer = (int) arg < 0;
            else
                throw new NotImplementedException ();
            //return interpreter.Apply (FixedObjectsVector.GenericNegativeP, new object [] { arg });
            return false;
        }

        [SchemePrimitive ("1+", 1)]
        public static bool Increment (out object answer, object arg)
        {
            if (arg is int)
                answer = (int) arg + 1;
            else
                throw new NotImplementedException ();
            // return interpreter.Apply (FixedObjectsVector.GenericAdd, new object [2] { arg, 1 });
            return false;
        }

        [SchemePrimitive ("-1+", 1)]
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
