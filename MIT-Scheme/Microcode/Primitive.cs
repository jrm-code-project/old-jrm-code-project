using System;
using System.Diagnostics;
using System.Collections.Generic;
using System.Globalization;
using System.Reflection;

namespace Microcode
{
    delegate object PrimitiveMethod (Interpreter interpreter, object [] arglist);

    delegate object PrimitiveMethod0 (Interpreter interpreter);
    delegate object PrimitiveMethod1 (Interpreter interpreter, object argument0);
    delegate object PrimitiveMethod2 (Interpreter interpreter, object argument0, object argument1);
    delegate object PrimitiveMethod3 (Interpreter interpreter, object argument0, object argument1, object argument2);

    public abstract class Primitive : SCode
    {
        public static bool Noisy = false;
        // Global table mapping names to primitive procedures.
        static Dictionary<string, Primitive> primitiveTable = new Dictionary<string, Primitive> ();

        protected readonly string name;
        readonly int arity;
        protected long invocationCount;

        internal Primitive (string name, int arity)
            : base (TC.PRIMITIVE)
        {
            this.name = name;
            this.arity = arity;
        }

        public int Arity
        {
            [DebuggerStepThrough]
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

        static void AddPrimitive (string name, int arity, PrimitiveMethod method)
        {
            primitiveTable.Add (CanonicalizeName (name), new PrimitiveN (String.Intern (name), arity, method));
        }

        static void AddPrimitive (string name, PrimitiveMethod0 method)
        {
            primitiveTable.Add (CanonicalizeName (name), new Primitive0 (String.Intern (name), method));
        }

        static void AddPrimitive (string name, PrimitiveMethod1 method)
        {
            primitiveTable.Add (CanonicalizeName (name), new Primitive1 (String.Intern (name), method));
        }

        static void AddPrimitive (string name, PrimitiveMethod2 method)
        {
            primitiveTable.Add (CanonicalizeName (name), new Primitive2 (String.Intern (name), method));
        }

        static void AddPrimitive (string name, PrimitiveMethod3 method)
        {
            primitiveTable.Add (CanonicalizeName (name), new Primitive3 (String.Intern (name), method));
        }

        static void AddPrimitives (Type type)
        {
            MemberInfo [] minfos = type.FindMembers (MemberTypes.Method, BindingFlags.Public | BindingFlags.Static, null, null);
            foreach (MemberInfo minfo in minfos)
            {
                object [] attributes = minfo.GetCustomAttributes (typeof (SchemePrimitiveAttribute), false);
                if (attributes.Length == 1)
                {
                    string name = ((SchemePrimitiveAttribute) (attributes [0])).Name;
                    int arity = ((SchemePrimitiveAttribute) (attributes [0])).Arity;
                    MethodInfo mtdinfo = (MethodInfo) minfo;
                    // Console.WriteLine ("Add Primitive {0}", ((PrimitiveAttribute) (attributes [0])).Name);
                    if (arity == 0)
                    {
                        PrimitiveMethod0 del = (PrimitiveMethod0) System.Delegate.CreateDelegate (typeof (PrimitiveMethod0), mtdinfo);
                        AddPrimitive (name, del);
                    }
                    else if (arity == 1)
                    {
                        PrimitiveMethod1 del = (PrimitiveMethod1) System.Delegate.CreateDelegate (typeof (PrimitiveMethod1), mtdinfo);
                        AddPrimitive (name, del);
                    }
                    else if (arity == 2)
                    {
                        PrimitiveMethod2 del = (PrimitiveMethod2) System.Delegate.CreateDelegate (typeof (PrimitiveMethod2), mtdinfo);
                        AddPrimitive (name, del);
                    }
                    else if (arity == 3)
                    {
                        PrimitiveMethod3 del = (PrimitiveMethod3) System.Delegate.CreateDelegate (typeof (PrimitiveMethod3), mtdinfo);
                        AddPrimitive (name, del);
                    }
                    else
                    {
                        PrimitiveMethod del = (PrimitiveMethod) System.Delegate.CreateDelegate (typeof (PrimitiveMethod), mtdinfo);
                        AddPrimitive (name, arity, del);
                    }
                }
            }
        }

        public static void Initialize ()
        {
            AddPrimitives (typeof (Bignum));
            AddPrimitives (typeof (BitString));
            AddPrimitives (typeof (Cell));
            AddPrimitives (typeof (IOPrims));
            AddPrimitives (typeof (Character));
            AddPrimitives (typeof (Complex));
            AddPrimitives (typeof (ControlPoint));
            AddPrimitives (typeof (Cons));
            AddPrimitives (typeof (Entity));
            AddPrimitives (typeof (Environment));
            AddPrimitives (typeof (Fasl));
            AddPrimitives (typeof (FixedObjectsVector));
            AddPrimitives (typeof (FixnumArithmetic));
            AddPrimitives (typeof (FloatArithmetic));
            AddPrimitives (typeof (FloatingVector));
            AddPrimitives (typeof (GenericArithmetic));
            AddPrimitives (typeof (History));
            AddPrimitives (typeof (Hunk3));
            AddPrimitives (typeof (IntegerArithmetic));
            AddPrimitives (typeof (Interpreter));
            AddPrimitives (typeof (Misc));
            AddPrimitives (typeof (ObjectModel));
            AddPrimitives (typeof (Primitive));
            AddPrimitives (typeof (Promise));
            AddPrimitives (typeof (Ratnum));
            AddPrimitives (typeof (Record));
            AddPrimitives (typeof (ReturnAddress));
            AddPrimitives (typeof (SchemeString));
            AddPrimitives (typeof (Vector));
            AddPrimitives (typeof (Vector8b));
        }

        internal static Primitive Find (string name)
        {
            Primitive value;
            if (primitiveTable.TryGetValue (CanonicalizeName(name), out value) == true) {
                    return value;
            }
            throw new NotImplementedException ();
        }

        internal static Primitive Find (string name, int arity)
        {
            Primitive value;
            if (primitiveTable.TryGetValue (CanonicalizeName(name), out value) == true) {
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

        [SchemePrimitive ("GET-PRIMITIVE-ADDRESS", 2)]
        public static object GetPrimitiveAddress (Interpreter interpreter, object arg0, object arg1)
        {
            if (arg1 is int)
                return interpreter.Return (Find ((string) arg0, (int) arg1));
            return interpreter.Return (Find ((string) arg0));
        }

        [SchemePrimitive ("GET-PRIMITIVE-NAME", 1)]
        public static object GetPrimitiveName (Interpreter interpreter, object arg)
        {
            return interpreter.Return (((Primitive) arg).name.ToCharArray ());
            //Primitive prim = (Primitive) arg;
            ////object prim;
            ////if (ObjectModel.datumObjectDictionary.TryGetValue ((int)arg, out prim)) {
            ////    ;
            ////}
            //throw new NotImplementedException ();
        }

        [SchemePrimitive ("PRIMITIVE-PROCEDURE-ARITY", 1)]
        public static object PrimitiveProcedureArity (Interpreter interpreter, object arg)
        {
            return interpreter.Return (((Primitive) (arg)).Arity);
        }

        internal override object EvalStep (Interpreter interpreter, object etc)
        {
            throw new NotImplementedException ();
        }

        internal override SCode Optimize (CompileTimeEnvironment ctenv)
        {
            throw new NotImplementedException ();
        }
    }

    sealed class Primitive0 : Primitive
    {
        PrimitiveMethod0 method;
        public Primitive0 (string name, PrimitiveMethod0 method)
            : base (name, 0)
        {
            this.method = method;
        }

        internal override object EvalStep (Interpreter interpreter, object etc)
        {
            Debug.WriteLineIf (Primitive.Noisy, this.name);
            this.invocationCount += 1;
            return this.method (interpreter);
        }
    }

    sealed class Primitive1 : Primitive
    {
        PrimitiveMethod1 method;

        public Primitive1 (string name, PrimitiveMethod1 method)
            : base (name, 1)
        {
            this.method = method;
        }

        internal override object EvalStep (Interpreter interpreter, object etc)
        {
            Debug.WriteLineIf (Primitive.Noisy, this.name);
            this.invocationCount += 1;
            return this.method (interpreter, interpreter.PrimitiveArgument0);
        }

    }

    sealed class Primitive2 : Primitive
    {
        PrimitiveMethod2 method;

        public Primitive2 (string name, PrimitiveMethod2 method)
            : base (name, 2)
        {
            this.method = method;
        }

        internal override object EvalStep (Interpreter interpreter, object etc)
        {
            Debug.WriteLineIf (Primitive.Noisy, this.name);
            this.invocationCount += 1;
            return this.method (interpreter, interpreter.PrimitiveArgument0, interpreter.PrimitiveArgument1);
        }
    }

    sealed class Primitive3 : Primitive
    {
        PrimitiveMethod3 method;

        public Primitive3 (string name, PrimitiveMethod3 method)
            : base (name, 3)
        {
            this.method = method;
        }

        internal override object EvalStep (Interpreter interpreter, object etc)
        {
            Debug.WriteLineIf (Primitive.Noisy, this.name);
            this.invocationCount += 1;
            return this.method (interpreter, interpreter.PrimitiveArgument0, interpreter.PrimitiveArgument1, interpreter.PrimitiveArgument2);
        }

    }

    sealed class PrimitiveN : Primitive
    {
        PrimitiveMethod method;

        public PrimitiveN (string name, int arity, PrimitiveMethod method)
            : base (name, arity)
        {
            this.method = method;
        }

        internal override object EvalStep (Interpreter interpreter, object etc)
        {
            Debug.WriteLineIf (Primitive.Noisy, this.name);
            this.invocationCount += 1;
            return this.method (interpreter, interpreter.Arguments);
        }
    }
}
