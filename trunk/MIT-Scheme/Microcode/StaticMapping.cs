using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;

namespace Microcode
{
    public struct StaticMapping
    {
        public Symbol name;
        public int offset;

        [DebuggerStepThrough]
        public StaticMapping (Symbol name, int offset)
        {
            this.name = name;
            this.offset = offset;
        }

        public static void ValidateStaticMapping (StaticMapping [] mapping)
        {
            if (mapping.Length > 1) {
                for (int i = 0; i < mapping.Length - 1; i++)
                    for (int j = i + 1; j < mapping.Length; j++)
                        if (mapping [i].name == mapping [j].name ||
                            mapping [i].offset == mapping [j].offset)
                            throw new NotImplementedException ();
            }
        }
    }
}
