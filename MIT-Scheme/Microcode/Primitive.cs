using System;
using System.Collections.Generic;
using System.Globalization;
using System.Linq;
using System.Text;

namespace Microcode
{
    delegate object PrimitiveMethod (Interpreter interpreter, object [] arglist);

    delegate object PrimitiveMethod0 (Interpreter interpreter);
    delegate object PrimitiveMethod1 (Interpreter interpreter, object argument0);
    delegate object PrimitiveMethod2 (Interpreter interpreter, object argument0, object argument1);
    delegate object PrimitiveMethod3 (Interpreter interpreter, object argument0, object argument1, object argument2);

    class Primitive
    {
        // Global table mapping names to primitive procedures.
        static Dictionary<String, Primitive> primitiveTable = new Dictionary<String, Primitive> ();

        readonly string name;
        readonly int arity;
        readonly PrimitiveMethod method;

        public Primitive (string name, int arity, PrimitiveMethod method)
        {
            this.name = name.ToUpperInvariant();
            this.arity = arity;
            this.method = method;
        }

        public int Arity
        {
            get
            {
                return this.arity;
            }
        }

        public override string ToString ()
        {
            return "#<PRIMITIVE " + this.name + " " + this.arity.ToString (CultureInfo.InvariantCulture) + ">";
        }

        static string CanonicalizeName (string name)
        {
            return String.Intern (name.ToUpperInvariant());
        }

        public static void AddPrimitive (string name, int arity, PrimitiveMethod method)
        {
            primitiveTable.Add (CanonicalizeName (name), new Primitive (String.Intern (name), arity, method));
        }

        public static void AddPrimitive (string name, PrimitiveMethod0 method)
        {
            primitiveTable.Add (CanonicalizeName (name), new Primitive0 (String.Intern (name), method));
        }

        public static void AddPrimitive (string name, PrimitiveMethod1 method)
        {
            primitiveTable.Add (CanonicalizeName (name), new Primitive1 (String.Intern (name), method));
        }

        public static void AddPrimitive (string name, PrimitiveMethod2 method)
        {
            primitiveTable.Add (CanonicalizeName (name), new Primitive2 (String.Intern (name), method));
        }

        public static void AddPrimitive (string name, PrimitiveMethod3 method)
        {
            primitiveTable.Add (CanonicalizeName (name), new Primitive3 (String.Intern (name), method));
        }

        internal static Primitive Find (string name)
        {
            Primitive value;
            if (primitiveTable.TryGetValue (CanonicalizeName (name), out value) == true) {
                    return value;
            }
            throw new NotImplementedException ();
        }

        internal static Primitive Find (string name, int arity)
        {
            Primitive value;
            if (primitiveTable.TryGetValue (CanonicalizeName (name), out value) == true) {
                // found one, but wrong arity
                if (value.Arity == arity)
                    return value;
                else
                    throw new NotImplementedException ();
            }
	// If we don't have the primitive in the table, fake one up with an
        // implementation that simply throws an error.  Saves time while
        // developing, but puts time bombs in the code!
            else if (arity == 0) {
                AddPrimitive (name, (PrimitiveMethod0) MissingPrimitive0);
                return Find (name, arity);
            }
            else if (arity == 1) {
                AddPrimitive (name, (PrimitiveMethod1) MissingPrimitive1);
                return Find (name, arity);
            }
            else if (arity == 2) {
                AddPrimitive (name, (PrimitiveMethod2) MissingPrimitive2);
                return Find (name, arity);
            }
            else if (arity == 3) {
                AddPrimitive (name, (PrimitiveMethod3) MissingPrimitive3);
                return Find (name, arity);
            }
            else {
                AddPrimitive (name, arity, (PrimitiveMethod) MissingPrimitive);
                return Find (name, arity);
            }
        }

        static object MissingPrimitive (Interpreter interpreter, object [] arglist)
        {
            throw new NotImplementedException ();
        }

        static object MissingPrimitive0 (Interpreter interpreter)
        {
            throw new NotImplementedException ();
        }

        static object MissingPrimitive1 (Interpreter interpreter, object arg)
        {
            throw new NotImplementedException ();
        }

        static object MissingPrimitive2 (Interpreter interpreter, object arg0, object arg1)
        {
            throw new NotImplementedException ();
        }

        static object MissingPrimitive3 (Interpreter interpreter, object arg0, object arg1, object arg2)
        {
            throw new NotImplementedException ();
        }
    }

    sealed class Primitive0 : Primitive
    {
        PrimitiveMethod0 code;

        public Primitive0 (String name, PrimitiveMethod0 code)
            : base (name, 0, null)
        {
            this.code = code;
        }
    }

    sealed class Primitive1 : Primitive
    {
        PrimitiveMethod1 code;

        public Primitive1 (String name, PrimitiveMethod1 code)
            : base (name, 1, null)
        {
            this.code = code;
        }
    }

    sealed class Primitive2 : Primitive
    {
        PrimitiveMethod2 code;

        public Primitive2 (String name, PrimitiveMethod2 code)
            : base (name, 2, null)
        {
            this.code = code;
        }
    }

    sealed class Primitive3 : Primitive
    {
        PrimitiveMethod3 code;

        public Primitive3 (String name, PrimitiveMethod3 code)
            : base (name, 3, null)
        {
            this.code = code;
        }
    }
}
