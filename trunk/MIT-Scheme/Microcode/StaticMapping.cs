using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;

namespace Microcode
{
    public class StaticMapping
    {
#if DEBUG
        static IList<int []> knownMappings = new List<int[]>();

        static bool SameMapping (int [] left, int [] right)
        {
            if (left.Length == right.Length) {
                for (int index = 0; index < left.Length; index++)
                    if (left [index] != right [index])
                        return false;
                return true;
            }
            return false;
        }
#endif
        Symbol [] names;
        int [] offsets;
        int offsetCode;

        public StaticMapping (Symbol [] names, int [] offsets)
        {
            int [] mapping = offsets;
            
#if DEBUG
            bool found = false;
            foreach (int [] knownMapping in knownMappings) {
               if (SameMapping(knownMapping, offsets)) {
                   mapping = knownMapping;
                   found = true;
                   break;
               }
            }
            if (!found)
                knownMappings.Add(mapping);
#endif
            this.names = names;
            this.offsets = mapping;
            foreach (int offset in offsets)
            {
                if (offset > 30)
                {
                    this.offsetCode = -1;
                    break;
                }
                this.offsetCode += 1 << offset;
            }
        }

        public Symbol[] Names { [DebuggerStepThrough] get { return this.names; } }
        public int [] Offsets { [DebuggerStepThrough] get { return this.offsets; } }
        public int OffsetCode { [DebuggerStepThrough] get { return this.offsetCode; } }

        public StaticMapping ()
        {
            this.names = new Symbol [0];
            this.offsets = new int [0];
        }

        public static void ValidateStaticMapping (StaticMapping staticMap)
        {
            throw new NotImplementedException ();
        }

        internal int Offset (object name)
        {
            for (int i = 0; i < this.names.Length; i++)
                if (name == this.names [i])
                    return i;
            return -1;
        }

        public int Size { [DebuggerStepThrough] get { return this.names.Length; } }

        internal int GetOffset (int index)
        {
            return this.offsets [index];
        }
    }
}