using System;

namespace Microcode
{
    sealed class Constant
    {
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

        public static Constant Unassigned
        {
            get
            {
                if (unassigned == null)
                    unassigned = new Constant ("Unassigned");
                return unassigned;
            }
        }

        public static Constant Unspecific
        {
            get
            {
                if (unspecific == null)
                    unspecific = new Constant ("Unspecific");
                return unspecific;
            }
        }
   }
}
