using System;
using System.Diagnostics;

namespace Microcode
{
    sealed class SchemePrimitiveAttribute : Attribute
    {
        readonly string name;
        readonly int arity;

        public SchemePrimitiveAttribute (string name, int arity)
            : base ()
        {
            this.name = name;
            this.arity = arity;
        }

        public string Name
        {
            [DebuggerStepThrough]
            get
            {
                return this.name;
            }
        }

        public int Arity
        {
            [DebuggerStepThrough]
            get
            {
                return this.arity;
            }
        }
    }
}
