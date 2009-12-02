using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Runtime.Serialization;
using System.Security.Permissions;

namespace Microcode
{
    [Serializable]
    public class Symbol : SchemeObject, ISerializable, ISystemPair
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public override TC TypeCode
        {
            get
            {
                return this.IsInterned() ? TC.INTERNED_SYMBOL : TC.UNINTERNED_SYMBOL;
            }
        }

        static Dictionary<string,Symbol> symbolTable = new Dictionary<string, Symbol> (25000);
        readonly string name;
        readonly int hashCode;
        static object [] obarray = new object [1];

        Symbol (string name, bool intern)
        {
            if (name == null) throw new ArgumentNullException ("name");
            if (intern) {
                this.name = String.Intern (name);
                symbolTable.Add (this.name, this);
                obarray [0] = new Cons (this, obarray [0]);
            }
            else {
                this.name = name;
            }
            this.hashCode = name.GetHashCode ();
        }

        protected Symbol (SerializationInfo info, StreamingContext context)
        {
            throw new NotImplementedException ();
        }

        static public Symbol Make (string name)
        {
            Symbol canonicalSymbol;
            if (symbolTable.TryGetValue (name, out canonicalSymbol) == false)
                canonicalSymbol = new Symbol (name, true);
            return canonicalSymbol;
        }

        static public object [] GetObarray ()
        {
            return obarray;
        }

        static public Symbol MakeUninterned (string name)
        {
            return new Symbol (name, false);
        }

        public char [] ToCharArray ()
        {
            return this.name.ToCharArray ();
        }

        [DebuggerStepThrough]
        public override string ToString ()
        {
            return this.name;
        }

        [DebuggerStepThrough]
        public override int GetHashCode ()
        {
            return this.hashCode;
        }

        public bool IsInterned ()
        {
            Symbol canonicalSymbol;
            return symbolTable.TryGetValue (this.name, out canonicalSymbol) &&
                    this == canonicalSymbol;
        }

        #region ISystemPair Members

        public object SystemPairCar
        {
            get
            {
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
                ValueCell cell = Environment.Global.GetValueCell(this);
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
        public virtual void GetObjectData (SerializationInfo info, StreamingContext context)
        {
            info.SetType (typeof (SymbolDeserializer));
            info.AddValue ("name", this.name);
            info.AddValue ("intern", this.TypeCode == TC.INTERNED_SYMBOL);
        }
    }

    [Serializable]
    internal sealed class SymbolDeserializer : IObjectReference
    {
        string name;
        Boolean intern;

        // GetRealObject is called after this object is deserialized.
        [SecurityPermissionAttribute (SecurityAction.LinkDemand, Flags = SecurityPermissionFlag.SerializationFormatter)]
        public Object GetRealObject (StreamingContext context)
        {
            return this.intern ?
                Symbol.Make (this.name) :
                Symbol.MakeUninterned (this.name);
        }

        // Muffle compiler
        string Name { set { this.name = value; } }
        Boolean Intern { set { this.intern = value; } }
    }
}
