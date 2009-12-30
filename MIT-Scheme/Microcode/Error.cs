using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    class Error
    {
        int code;
        protected Error (int code) {
            this.code = code;
        }

        public object Handler
        {
            get
            {
                return FixedObjectsVector.ErrorVector [this.code];
            }
        }

        internal static bool WrongNumberOfArguments (out object answer, ref Control expression, ref Environment environment)
        {
            throw new NotImplementedException ();
        }
    }

    class UnboundVariableError : Error
    {
        Symbol name;

        public UnboundVariableError (Symbol name)
            : base (1)
        {
            this.name = name;
        }
    }

}
