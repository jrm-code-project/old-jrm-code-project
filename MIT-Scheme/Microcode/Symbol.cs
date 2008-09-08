using System;
using System.Collections.Generic;


namespace Microcode
{
    public class Symbol : SchemeObject
    {
        static Dictionary<string,Symbol> symbolTable = new Dictionary<string, Symbol> ();
        readonly string name;

        Symbol (string name, bool intern)
            : base (intern ? TC.INTERNED_SYMBOL : TC.UNINTERNED_SYMBOL)
        {
            this.name = name;
            if (intern)
                symbolTable.Add(name, this);
        }

        static public Symbol Make (string name)
        {
            string canonicalName = String.Intern (name);
            Symbol canonicalSymbol;
            if (symbolTable.TryGetValue (name, out canonicalSymbol) == false)
                canonicalSymbol = new Symbol (canonicalName, true);
            return canonicalSymbol;
        }

        public static implicit operator Symbol (string name)
        {
            return Make (name);
        }

        static public Symbol MakeUninterned (string name)
        {
            return new Symbol (name, false);
        }

        public char [] ToCharArray ()
        {
            return this.name.ToCharArray ();
        }

        public override string ToString ()
        {
            return this.name;
        }
    }
}
