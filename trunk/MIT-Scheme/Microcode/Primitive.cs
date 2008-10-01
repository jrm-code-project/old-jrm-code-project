using System;
using System.Diagnostics;
using System.Collections.Generic;
using System.Globalization;
using System.Reflection;
using System.Runtime.Serialization;
using System.Security.Permissions;

namespace Microcode
{
    delegate bool PrimitiveMethod (out object answer, object [] arglist);

    delegate bool PrimitiveMethod0 (out object answer);
    delegate bool PrimitiveMethod1 (out object answer, object argument0);
    delegate bool PrimitiveMethod2 (out object answer, object argument0, object argument1);
    delegate bool PrimitiveMethod3 (out object answer, object argument0, object argument1, object argument2);

    [Serializable]
    public abstract class Primitive : SchemeObject //SCode
    {
#if DEBUG
        [NonSerialized]
        public static bool Noisy = false;

        [NonSerialized]
        internal static Histogram<Primitive> hotPrimitives = new Histogram<Primitive> ();
#endif

        // Global table mapping names to primitive procedures.
        [NonSerialized]
        static Dictionary<string, Primitive> primitiveTable = new Dictionary<string, Primitive> ();

        protected readonly string name;
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly int arity;

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
            return String.Intern (name.ToUpperInvariant ());
        }

        static void AddPrimitive (string name, int arity, PrimitiveMethod method)
        {
            string cname = CanonicalizeName (name);
            primitiveTable.Add (cname, new PrimitiveN (String.Intern (name), arity, method));
        }

        static void AddPrimitive (string name, PrimitiveMethod0 method)
        {
            string cname = CanonicalizeName (name);
            primitiveTable.Add (cname, new Primitive0 (String.Intern (name), method));
        }

        static void AddPrimitive (string name, PrimitiveMethod1 method)
        {
            string cname = CanonicalizeName (name);
            primitiveTable.Add (cname, new Primitive1 (String.Intern (name), method));
        }

        static void AddPrimitive (string name, PrimitiveMethod2 method)
        {
            string cname = CanonicalizeName (name);
            primitiveTable.Add (cname, new Primitive2 (String.Intern (name), method));
        }

        static void AddPrimitive (string name, PrimitiveMethod3 method)
        {
            primitiveTable.Add (CanonicalizeName (name), new Primitive3 (String.Intern (name), method));
        }

        static void AddPrimitives (Type type)
        {
            MemberInfo [] minfos = type.FindMembers (MemberTypes.Method, BindingFlags.Public | BindingFlags.Static, null, null);
            foreach (MemberInfo minfo in minfos) {
                object [] attributes = minfo.GetCustomAttributes (typeof (SchemePrimitiveAttribute), false);
                if (attributes.Length == 1) {
                    string name = ((SchemePrimitiveAttribute) (attributes [0])).Name;
                    int arity = ((SchemePrimitiveAttribute) (attributes [0])).Arity;
                    MethodInfo mtdinfo = (MethodInfo) minfo;
                    // Console.WriteLine ("Add Primitive {0}", ((PrimitiveAttribute) (attributes [0])).Name);
                    if (arity == 0) {
                        PrimitiveMethod0 del = (PrimitiveMethod0) System.Delegate.CreateDelegate (typeof (PrimitiveMethod0), mtdinfo);
                        AddPrimitive (name, del);
                    }
                    else if (arity == 1) {
                        PrimitiveMethod1 del = (PrimitiveMethod1) System.Delegate.CreateDelegate (typeof (PrimitiveMethod1), mtdinfo);
                        AddPrimitive (name, del);
                    }
                    else if (arity == 2) {
                        PrimitiveMethod2 del = (PrimitiveMethod2) System.Delegate.CreateDelegate (typeof (PrimitiveMethod2), mtdinfo);
                        AddPrimitive (name, del);
                    }
                    else if (arity == 3) {
                        PrimitiveMethod3 del = (PrimitiveMethod3) System.Delegate.CreateDelegate (typeof (PrimitiveMethod3), mtdinfo);
                        AddPrimitive (name, del);
                    }
                    else {
                        PrimitiveMethod del = (PrimitiveMethod) System.Delegate.CreateDelegate (typeof (PrimitiveMethod), mtdinfo);
                        AddPrimitive (name, arity, del);
                    }
                }
            }
        }

        public static void Initialize ()
        {
            Assembly asm = Assembly.GetExecutingAssembly ();
            foreach (Type type in asm.GetTypes ())
                AddPrimitives (type);
            //AddPrimitives (typeof (Bignum));
            //AddPrimitives (typeof (BitString));
            //AddPrimitives (typeof (Cell));
            //AddPrimitives (typeof (IOPrims));
            //AddPrimitives (typeof (Character));
            //AddPrimitives (typeof (Closure));
            //AddPrimitives (typeof (Combination));
            //AddPrimitives (typeof (Combination1));
            //AddPrimitives (typeof (Combination2));
            //AddPrimitives (typeof (Complex));
            //AddPrimitives (typeof (ControlPoint));
            //AddPrimitives (typeof (Cons));
            //AddPrimitives (typeof (Entity));
            //AddPrimitives (typeof (Environment));
            //AddPrimitives (typeof (ExtendedClosure));
            //AddPrimitives (typeof (Fasl));
            //AddPrimitives (typeof (FixedObjectsVector));
            //AddPrimitives (typeof (FixnumArithmetic));
            //AddPrimitives (typeof (FloatArithmetic));
            //AddPrimitives (typeof (FloatingVector));
            //AddPrimitives (typeof (GenericArithmetic));
            //AddPrimitives (typeof (History));
            //AddPrimitives (typeof (Hunk3));
            //AddPrimitives (typeof (IntegerArithmetic));
            //AddPrimitives (typeof (Interpreter));
            //AddPrimitives (typeof (Lambda));
            //AddPrimitives (typeof (Misc));
            //AddPrimitives (typeof (NonMarkedVector));
            //AddPrimitives (typeof (ObjectModel));
            //AddPrimitives (typeof (Primitive));
            //AddPrimitives (typeof (PrimitiveCombination0));
            //AddPrimitives (typeof (PrimitiveCombination1));
            //AddPrimitives (typeof (PrimitiveCombination2));
            //AddPrimitives (typeof (PrimitiveCombination3));
            //AddPrimitives (typeof (Promise));
            //AddPrimitives (typeof (Ratnum));
            //AddPrimitives (typeof (Record));
            //AddPrimitives (typeof (ReferenceTrap));
            //AddPrimitives (typeof (ReturnAddress));
            //AddPrimitives (typeof (SchemeString));
            //AddPrimitives (typeof (SystemPair));
            //AddPrimitives (typeof (UnmarkedHistory));
            //AddPrimitives (typeof (Vector));
            //AddPrimitives (typeof (Vector8b));
        }

        internal static Primitive Find (string name)
        {
            string cname = CanonicalizeName (name);
            Primitive value;
            if (primitiveTable.TryGetValue (cname, out value)) {
                return value;
            }
            throw new NotImplementedException ();
        }

        internal static Primitive Find (string name, int arity)
        {
            string cname = CanonicalizeName (name);
            Primitive value;
            if (primitiveTable.TryGetValue (cname, out value)) {
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

        static bool MissingPrimitive (out object answer, object [] arglist)
        {
            throw new NotImplementedException ();
        }

        static bool MissingPrimitive0 (out object answer)
        {
            throw new NotImplementedException ();
        }

        static bool MissingPrimitive1 (out object answer, object arg)
        {
            throw new NotImplementedException ();
        }

        static bool MissingPrimitive2 (out object answer, object arg0, object arg1)
        {
            throw new NotImplementedException ();
        }

        static bool MissingPrimitive3 (out object answer, object arg0, object arg1, object arg2)
        {
            throw new NotImplementedException ();
        }

        [SchemePrimitive ("GET-PRIMITIVE-ADDRESS", 2, false)]
        public static bool GetPrimitiveAddress (out object answer, object arg0, object arg1)
        {
            answer = arg1 is int
                ? Find ((string) arg0, (int) arg1)
                : Find ((string) arg0);
            return false;
        }

        [SchemePrimitive ("GET-PRIMITIVE-NAME", 1, false)]
        public static bool GetPrimitiveName (out object answer, object arg)
        {
            answer = ((Primitive) arg).name.ToCharArray ();
            return false;
        }

        [SchemePrimitive ("PRIMITIVE?", 1, true)]
        public static bool IsPrimitive (out object answer, object arg)
        {
            answer = arg is Primitive;
            return false;
        }


        [SchemePrimitive ("PRIMITIVE-PROCEDURE-ARITY", 1, false)]
        public static bool PrimitiveProcedureArity (out object answer, object arg)
        {
            answer = ((Primitive) arg).Arity;
            return false;
        }

    }

    [Serializable]
    sealed class Primitive0 : Primitive, IApplicable, ISerializable
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly PrimitiveMethod0 method;

        public Primitive0 (string name, PrimitiveMethod0 method)
            : base (name, 0)
        {
            this.method = method;
        }

        public PrimitiveMethod0 Method
        {
            [DebuggerStepThrough]
            get
            {
                return this.method;
            }
        }

        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        void ISerializable.GetObjectData (SerializationInfo info, StreamingContext context)
        {
            info.SetType (typeof (PrimitiveDeserializer));
            info.AddValue ("name", this.name);
            info.AddValue ("arity", 0);
        }

        #region IApplicable Members

        public bool Apply (out object answer, ref Control expression, ref Environment environment, object [] args)
        {
            if (args.Length != 0)
                throw new NotImplementedException ("Wrong number of args to primitive.");
            return this.Call (out answer, ref expression, ref environment);
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Debug.WriteLineIf (Primitive.Noisy, this.name);
            Primitive.hotPrimitives.Note (this);
#endif
            if (this.method (out answer)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci == null) throw new NotImplementedException ();
                answer = null;      
                expression = tci.Expression;
                environment = tci.Environment;
                return true;
            }

            return false; // no problems
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0)
        {
            throw new NotImplementedException ();
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1)
        {
            throw new NotImplementedException ();
        }

        #endregion

        #region IApplicable Members


        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2)
        {
            throw new NotImplementedException ();
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3)
        {
            throw new NotImplementedException ();
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3, object arg4)
        {
            throw new NotImplementedException ();
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3, object arg4, object arg5)
        {
            throw new NotImplementedException ();
        }

        #endregion
    }

    [Serializable]
    sealed class Primitive1 : Primitive, IApplicable, ISerializable
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly PrimitiveMethod1 method;

        public Primitive1 (string name, PrimitiveMethod1 method)
            : base (name, 1)
        {
            this.method = method;
        }


        public PrimitiveMethod1 Method
        {
            [DebuggerStepThrough]
            get
            {
                return this.method;
            }
        }

        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        void ISerializable.GetObjectData (SerializationInfo info, StreamingContext context)
        {
            info.SetType (typeof (PrimitiveDeserializer));
            info.AddValue ("name", this.name);
            info.AddValue ("arity", 1);
        }

        #region IApplicable Members

        public bool Apply (out object answer, ref Control expression, ref Environment environment, object [] args)
        {
            throw new NotImplementedException ();
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0)
        {
#if DEBUG
            Debug.WriteLineIf (Primitive.Noisy, this.name);
            hotPrimitives.Note (this);
#endif
            if (this.method (out answer, arg0))
                throw new NotImplementedException ();
            return false; // no problems
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1)
        {
            throw new NotImplementedException ();
        }

        #endregion

        #region IApplicable Members


        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2)
        {
            throw new NotImplementedException ();
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3)
        {
            throw new NotImplementedException ();
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3, object arg4)
        {
            throw new NotImplementedException ();
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3, object arg4, object arg5)
        {
            throw new NotImplementedException ();
        }

        #endregion
    }

    [Serializable]
    sealed class Primitive2 : Primitive, IApplicable, ISerializable
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly PrimitiveMethod2 method;

        public Primitive2 (string name, PrimitiveMethod2 method)
            : base (name, 2)
        {
            this.method = method;
        }

        public PrimitiveMethod2 Method
        {
            [DebuggerStepThrough]
            get
            {
                return this.method;
            }
        }

        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        void ISerializable.GetObjectData (SerializationInfo info, StreamingContext context)
        {
            info.SetType (typeof (PrimitiveDeserializer));
            info.AddValue ("name", this.name);
            info.AddValue ("arity", 2);
        }

        #region IApplicable Members

        public bool Apply (out object answer, ref Control expression, ref Environment environment,object [] args)
        {
            throw new NotImplementedException ();
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0)
        {
            throw new NotImplementedException ();
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1)
        {
#if DEBUG
            Debug.WriteLineIf (Primitive.Noisy, this.name);
            hotPrimitives.Note (this);
#endif
            if (this.method (out answer, arg0, arg1)) {
                TailCallInterpreter tci = answer as TailCallInterpreter;
                if (tci == null) throw new NotImplementedException ();
                answer = null;
                expression = tci.Expression;
                environment = tci.Environment;
                return true;
            }

            return false;
        }

        #endregion

        #region IApplicable Members


        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2)
        {
            throw new NotImplementedException ();
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3)
        {
            throw new NotImplementedException ();
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3, object arg4)
        {
            throw new NotImplementedException ();
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3, object arg4, object arg5)
        {
            throw new NotImplementedException ();
        }

        #endregion
    }

    [Serializable]
    sealed class Primitive3 : Primitive, IApplicable, ISerializable
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly PrimitiveMethod3 method;

        public Primitive3 (string name, PrimitiveMethod3 method)
            : base (name, 3)
        {
            this.method = method;
        }

        public PrimitiveMethod3 Method
        {
            [DebuggerStepThrough]
            get
            {
                return this.method;
            }
        }

        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        void ISerializable.GetObjectData (SerializationInfo info, StreamingContext context)
        {
            info.SetType (typeof (PrimitiveDeserializer));
            info.AddValue ("name", this.name);
            info.AddValue ("arity", 3);
        }

        #region IApplicable Members

        public bool Apply (out object answer, ref Control expression, ref Environment environment, object [] args)
        {
            throw new NotImplementedException ();
        }

        public bool Call (out object anwswer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0)
        {
            throw new NotImplementedException ();
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1)
        {
            throw new NotImplementedException ();
        }

        #endregion

        #region IApplicable Members


        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2)
        {
            throw new NotImplementedException ();
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3)
        {
            throw new NotImplementedException ();
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3, object arg4)
        {
            throw new NotImplementedException ();
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3, object arg4, object arg5)
        {
            throw new NotImplementedException ();
        }

        #endregion
    }

    [Serializable]
    sealed class PrimitiveN : Primitive, IApplicable, ISerializable
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly PrimitiveMethod method;

        public PrimitiveN (string name, int arity, PrimitiveMethod method)
            : base (name, arity)
        {
            this.method = method;
        }

        public PrimitiveMethod Method
        {
            [DebuggerStepThrough]
            get
            {
                return this.method;
            }
        }

        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        void ISerializable.GetObjectData (SerializationInfo info, StreamingContext context)
        {
            info.SetType (typeof (PrimitiveDeserializer));
            info.AddValue ("name", this.name);
            info.AddValue ("arity", this.Arity);
        }

        #region IApplicable members

        public bool Apply (out object answer, ref Control expression, ref Environment environment, object [] args)
        {
#if DEBUG
            Debug.WriteLineIf (Primitive.Noisy, this.name);
            Primitive.hotPrimitives.Note (this);
#endif
            //// gotta remove the procedure from the front of the arglist.
            //object [] args1 = new object [args.Length - 1];
            //Array.Copy (args, 1, args1, 0, args1.Length);
            if (args != null && args.Length > 0 && args [0] == this)
                throw new NotImplementedException ();

            if (this.method (out answer, args))
                throw new NotImplementedException ();
            return false; // no problems
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment)
        {
#if DEBUG
            Debug.WriteLineIf (Primitive.Noisy, this.name);
            hotPrimitives.Note (this);
#endif
            object [] arguments = new object [] { };
            if (this.method (out answer, arguments))
                throw new NotImplementedException ();
            return false; // no problems
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0)
        {
#if DEBUG
            Debug.WriteLineIf (Primitive.Noisy, this.name);
            hotPrimitives.Note (this);
#endif
            object [] arguments = new object [] { arg0 };
            if (this.method (out answer, arguments))
                throw new NotImplementedException ();
            return false; // no problems
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1)
        {
#if DEBUG
            Debug.WriteLineIf (Primitive.Noisy, this.name);
            hotPrimitives.Note (this);
#endif
            object [] arguments = new object [] { arg0, arg1 };
            if (this.method (out answer, arguments))
                throw new NotImplementedException ();
            return false; // no problems
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2)
        {
#if DEBUG
            Debug.WriteLineIf (Primitive.Noisy, this.name);
            hotPrimitives.Note (this);
#endif
            object [] arguments = new object [] { arg0, arg1, arg2 };
            if (this.method (out answer, arguments))
                throw new NotImplementedException ();
            return false; // no problems
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3)
        {
#if DEBUG
            Debug.WriteLineIf (Primitive.Noisy, this.name);
            hotPrimitives.Note (this);
#endif
            object [] arguments = new object [] { arg0, arg1, arg2, arg3};
            if (this.method (out answer, arguments))
                throw new NotImplementedException ();
            return false; // no problems   
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3, object arg4)
        {
            throw new NotImplementedException ();
        }

        public bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3, object arg4, object arg5)
        {
            throw new NotImplementedException ();
        }


        #endregion

        #region IApplicable Members



        #endregion
    }

    [Serializable]
    internal sealed class PrimitiveDeserializer : IObjectReference
    {
        // This object has no fields (although it could).
        string name;
        int arity;

        // GetRealObject is called after this object is deserialized.
        public Object GetRealObject (StreamingContext context)
        {
            return Primitive.Find (this.name, this.arity);
        }

        public void SetName (string value) { this.name = value; }
        public void SetArity (int value) { this.arity = value; }
    }

}
