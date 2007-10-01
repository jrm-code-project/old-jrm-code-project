using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Lisp
{
    [CLSCompliant (true)]
    [System.Diagnostics.DebuggerDisplay ("{Package}::{Name}")]
    public class Symbol
    {
        [System.Diagnostics.DebuggerBrowsable (System.Diagnostics.DebuggerBrowsableState.Never)]
        readonly string name;
        [System.Diagnostics.DebuggerBrowsable (System.Diagnostics.DebuggerBrowsableState.Never)]
        readonly Package package;

        public Symbol (string name)
        {
            if (name == null)
                throw new ArgumentNullException ("name");
            this.name = name;
            // this.package = null;
        }

        public Symbol (string name, Package package)
        {
            if (name == null)
                throw new ArgumentNullException ("name");
            this.name = name;
            this.package = package;
        }

        public string Name
        {
            [System.Diagnostics.DebuggerStepThrough ()]
            get
            {
                return this.name;
            }
        }

        public Package Package
        {
            [System.Diagnostics.DebuggerStepThrough ()]
            get
            {
                return this.package;
            }
        }

        public bool NamesDotnetMethod ()
        {
            return this.name.StartsWith (".") && !this.name.EndsWith ("$");
        }
    }
}
