using System;
using System.Diagnostics;

namespace Microcode
{
    sealed class Constant
    {
        static Constant optional;
        static Constant rest;
        static Constant unassigned;
        static Constant unspecific;

        string name;

        private Constant (string name)
        {
            this.name = name;
        }

        public override string ToString ()
        {
            return "#!" + this.name;
        }

        public static Constant LambdaOptionalTag
        {
            [DebuggerStepThrough]
            get
            {
                if (optional == null)
                    optional = new Constant ("optional");
                return optional;
            }
        }

        public static Constant LambdaRestTag
        {
            [DebuggerStepThrough]
            get
            {
                if (rest == null)
                    rest = new Constant ("rest");
                return rest;
            }
        }

        public static Constant Unassigned
        {
            [DebuggerStepThrough]
            get
            {
                if (unassigned == null)
                    unassigned = new Constant ("Unassigned");
                return unassigned;
            }
        }

        public static Constant Unspecific
        {
            [DebuggerStepThrough]
            get
            {
                if (unspecific == null)
                    unspecific = new Constant ("Unspecific");
                return unspecific;
            }
        }
   }
}
