using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    static class FixnumArithmetic
    {
        [SchemePrimitive ("FIXNUM?", 1)]
        public static object IsFixnum (Interpreter interpreter, object arg0)
        {
            return interpreter.Return (arg0 is Int32);
        }

        [SchemePrimitive ("INDEX-FIXNUM?", 1)]
        public static object IsIndexFixnum (Interpreter interpreter, object arg0)
        {
            return interpreter.Return (arg0 is Int32 && ((int) arg0 >= 0));
        }

        [SchemePrimitive ("ZERO-FIXNUM?", 1)]
        public static object ZeroP (Interpreter interpreter, object arg0)
        {
            return interpreter.Return (arg0 is Int32 && (int) arg0 == 0);
        }

        [SchemePrimitive ("NEGATIVE-FIXNUM?", 1)]
        public static object NegativeP (Interpreter interpreter, object arg0)
        {
            return interpreter.Return ((int) arg0 < 0);
        }

        [SchemePrimitive ("POSITIVE-FIXNUM?", 1)]
        public static object PositiveP (Interpreter interpreter, object arg0)
        {
            return interpreter.Return ((int) arg0 > 0);
        }

        [SchemePrimitive ("EQUAL-FIXNUM?", 2)]
        public static object EqualP (Interpreter interpreter, object left, object right)
        {
            return interpreter.Return ((int) left == (int) right);
        }

        [SchemePrimitive ("LESS-THAN-FIXNUM?", 2)]
        public static object LessP (Interpreter interpreter, object left, object right)
        {
            return interpreter.Return ((int) left < (int) right);
        }

        [SchemePrimitive ("GREATER-THAN-FIXNUM?", 2)]
        public static object GreaterP (Interpreter interpreter, object left, object right)
        {
            return interpreter.Return ((int) left > (int) right);
        }

        [SchemePrimitive ("ONE-PLUS-FIXNUM", 1)]
        public static object Add1 (Interpreter interpreter, object arg0)
        {
            return interpreter.Return ((int) arg0 + 1);
        }

        [SchemePrimitive ("MINUS-ONE-PLUS-FIXNUM", 1)]
        public static object Subtract1 (Interpreter interpreter, object arg0)
        {
            return interpreter.Return ((int) arg0 - 1);
        }

        [SchemePrimitive ("PLUS-FIXNUM", 2)]
        public static object Add (Interpreter interpreter, object left, object right)
        {
            return interpreter.Return ((int) left + (int) right);
        }

        [SchemePrimitive ("MINUS-FIXNUM", 2)]
        public static object Subtract (Interpreter interpreter, object left, object right)
        {
            return interpreter.Return ((int) left - (int) right);
        }

        [SchemePrimitive ("FIXNUM-NEGATE", 1)]
        public static object Negate (Interpreter interpreter, object arg0)
        {
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("MULITPLY-FIXNUM", 2)]
        public static object Multiply (Interpreter interpreter, object left, object right)
        {
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("DIVIDE-FIXNUM", 2)]
        public static object Divide (Interpreter interpreter, object left, object right)
        {
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("FIXNUM-QUOTIENT", 2)]
        public static object Quotient (Interpreter interpreter, object left, object right)
        {
            return interpreter.Return ((int) left / (int) right);
        }

        [SchemePrimitive ("FIXNUM-REMAINDER", 2)]
        public static object Remainder (Interpreter interpreter, object left, object right)
        {
            return interpreter.Return ((int) left % (int) right);
        }

        [SchemePrimitive ("GCD-FIXNUM", 2)]
        public static object GreatestCommonDivisor (Interpreter interpreter, object left, object right)
        {
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("FIXNUM-ANDC", 2)]
        public static object AndC (Interpreter interpreter, object left, object right)
        {
            return interpreter.Return ((int) left & ~((int) right));
        }

        [SchemePrimitive ("FIXNUM-AND", 2)]
        public static object And (Interpreter interpreter, object left, object right)
        {
            return interpreter.Return ((int) left & (int) right);
        }

        [SchemePrimitive ("FIXNUM-OR", 2)]
        public static object Or (Interpreter interpreter, object left, object right)
        {
            return interpreter.Return ((int) left | (int) right);
        }

        [SchemePrimitive ("FIXNUM-XOR", 2)]
        public static object Xor (Interpreter interpreter, object left, object right)
        {
            return interpreter.Return ((int) left ^ (int) right);
        }

        [SchemePrimitive ("FIXNUM-NOT", 1)]
        public static object Not (Interpreter interpreter, object arg)
        {
            return interpreter.Return (~((int) arg));
        }

        [SchemePrimitive ("FIXNUM-LSH", 2)]
        public static object Lsh (Interpreter interpreter, object left, object right)
        {
            return interpreter.Return ((int) left << (int) right);
        }

        [SchemePrimitive ("FIXNUM->FLONUM", 1)]
        public static object ToFlonum (Interpreter interpreter, object arg)
        {
            return interpreter.Return ((double) (int) arg);
        }
    }
}
