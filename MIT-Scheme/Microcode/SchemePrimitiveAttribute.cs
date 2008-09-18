using System;
using System.Diagnostics;

namespace Microcode
{
    sealed class SchemePrimitiveAttribute : Attribute
    {
        readonly string name;
        readonly int arity;
        // Total is true iff function cannot error or tail call.
        readonly bool total;

        public SchemePrimitiveAttribute (string name, int arity, bool total)
            : base ()
        {
            this.name = name;
            this.arity = arity;
            this.total = total;
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

        public bool Total
        {
            [DebuggerStepThrough]
            get
            {
                return this.total;
            }
        }
    }
}
