using System;
using System.Diagnostics;

namespace Microcode
{
    enum TrapKind
    {
        NON_TRAP_KIND = 32,
        TRAP_COMPILER_CACHED = 14,
        TRAP_EXPENSIVE = 6,
        TRAP_MACRO = 15,
        TRAP_UNASSIGNED = 0,
        TRAP_UNBOUND = 2
    }



    class ReferenceTrap
    {
        public static TrapKind GetTrapKind (object value)
        {
            ReferenceTrap reftrap = value as ReferenceTrap;
            if (reftrap == null)
                return TrapKind.NON_TRAP_KIND;
            else if (reftrap == unassigned)
                return TrapKind.TRAP_UNASSIGNED;
            else 
                return (TrapKind)(((Cons)(reftrap.contents)).Car);
        }

        static ReferenceTrap expensive;
        static ReferenceTrap unassigned;
        static ReferenceTrap unbound;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        object contents;

        public ReferenceTrap ()
        {
            contents = null;
        }

        public ReferenceTrap (object obj)
        {
            this.contents = obj;
        }

        public object Contents
        {
            [DebuggerStepThrough]
            get
            {
                return this.contents;
            }
        }

        public static ReferenceTrap Expensive
        {
            get
            {
                if (expensive == null)
                    expensive = new ReferenceTrap ();
                return expensive;
            }
        }

        public static ReferenceTrap Unassigned
        {
            get
            {
                if (unassigned == null)
                    unassigned = new ReferenceTrap ();
                return unassigned;
            }
        }

        public static ReferenceTrap Unbound
        {
            get
            {
                if (unbound == null)
                    unbound = new ReferenceTrap ();
                return unbound;
            }
        }

        public static ReferenceTrap Make (object arg)
        {
            if (arg is int)
                return Make ((int) arg);
            else if (arg is Cons)
                return Make ((Cons) arg);
            else
                throw new NotImplementedException ();
        }

        public static ReferenceTrap Make (int arg)
        {
            if (arg == 0)
                return Unassigned;
            else if (arg == 2)
                return Unbound;
            else
                throw new NotImplementedException ();
        }

        public static ReferenceTrap Make (Cons arg)
        {
            return new ReferenceTrap (arg);
        }

    }
}
