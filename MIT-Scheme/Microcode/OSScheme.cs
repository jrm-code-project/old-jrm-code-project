using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    static class OSScheme
    {
        static int pendingInterrupts;
        public static bool IsPendingInterrupts ()
        {
            return pendingInterrupts != 0;
        }

        static public bool RaiseInterrupt (int interrupt)
        {
            // wrong, but it muffles the compiler warning.
            pendingInterrupts = 1;
            throw new NotImplementedException();
        }
    }
}
