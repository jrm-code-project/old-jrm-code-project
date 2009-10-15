using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;

namespace Microcode
{
    sealed class Statistics
    {
        [SchemePrimitive ("RESET-STATISTICS!", 0, false)]
        public static bool Reset (out object answer)
        {
#if DEBUG
            Environment.foundAtDepth = new long [128];
            Environment.extendedBy = new long [1024];

            SCode.evaluations = 0;
            SCode.hotSCode.Clear();
            SCode.scodeHistogram.Clear();
            SCode.topOfStack.Clear();
            SCode.callTable.Clear();
#endif
            Debug.WriteLine (";; Reset statistics.");
            answer = Constant.Unspecific;
            return false;
        }
    }
}
