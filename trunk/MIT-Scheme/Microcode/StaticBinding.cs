using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;

namespace Microcode
{
    /// <summary>
    /// Represents the formal parameter of a lambda expression.
    /// </summary>
    sealed class StaticBinding
    {
        // Name is an object because it might
        // be a gensym.
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        object name;
        ValueCell cell; // the lambda expression that binds it

        public StaticBinding (object name, ValueCell cell)
        {
            this.cell = cell;
            this.name = name;
        }

        public object Name
        {
            get
            {
                return this.name;
            }
        }
        public ValueCell Cell
        {
            get
            {
                return this.cell;
            }
        }
    }
}
