using System;
using System.Collections.Generic;
using System.Runtime.Serialization;
using System.Security.Permissions;

namespace Microcode
{
    [Serializable]
    public class Symbol : SchemeObject, ISerializable, ISystemPair
    {
        static Dictionary<string,Symbol> symbolTable = new Dictionary<string, Symbol> ();
        readonly string name;
        readonly int hashCode;
        static object [] obarray = new object [1];

        Symbol (string name, bool intern)
            : base (intern ? TC.INTERNED_SYMBOL : TC.UNINTERNED_SYMBOL)
        {
            this.name = name;
            this.hashCode = name.GetHashCode ();
            if (intern) {
                symbolTable.Add (name, this);
                obarray [0] = new Cons (this, obarray [0]);
            }
        }

        static public Symbol Make (string name)
        {
            string canonicalName = String.Intern (name);
            Symbol canonicalSymbol;
            if (symbolTable.TryGetValue (name, out canonicalSymbol) == false)
                canonicalSymbol = new Symbol (canonicalName, true);
            return canonicalSymbol;
        }

        static public object [] GetObarray ()
        {
            return obarray;
        }

        void CheckInterning ()
        {
            Symbol canonicalSymbol;
            if (symbolTable.TryGetValue (this.name, out canonicalSymbol) == false ||
                this != canonicalSymbol) {
                if (this.TypeCode != TC.UNINTERNED_SYMBOL)
                    throw new NotImplementedException();
            }
            else {
                if (this.TypeCode != TC.INTERNED_SYMBOL)
                    throw new NotImplementedException();
            }
        }

        static public Symbol MakeUninterned (string name)
        {
            return new Symbol (name, false);
        }

        public char [] ToCharArray ()
        {
#if DEBUG
            CheckInterning();
#endif
            return this.name.ToCharArray ();
        }

        public override string ToString ()
        {
            return this.name;
        }

        public override int GetHashCode ()
        {
            return this.hashCode;
        }

        #region ISystemPair Members

        public object SystemPairCar
        {
            get
            {
#if DEBUG
            CheckInterning();
#endif
                return this.name.ToCharArray();
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        public object SystemPairCdr
        {
            get
            {
                ValueCell cell = Environment.Global.GetValueCell(this.name);
                if (cell == null) return ReferenceTrap.Unbound;
                object result;
                cell.GetValue(out result);
                return result;
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        #endregion

        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        void ISerializable.GetObjectData (SerializationInfo info, StreamingContext context)
        {
            info.SetType (typeof (SymbolDeserializer));
            info.AddValue ("name", this.name);
            info.AddValue ("intern", this.TypeCode == TC.INTERNED_SYMBOL);
        }
    }

    [Serializable]
    internal sealed class SymbolDeserializer : IObjectReference
    {
        // This object has no fields (although it could).
        string name;
        bool intern;

        // GetRealObject is called after this object is deserialized.
        public Object GetRealObject (StreamingContext context)
        {
            if (intern)
                return Symbol.Make (this.name);
            else
                return Symbol.MakeUninterned (this.name);
        }

        public void SetName (string value) { this.name = value; }
        public void SetIntern (bool value) { this.intern = value; }
    }
}
