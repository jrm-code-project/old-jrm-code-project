using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    class Cell
    {
        [SchemePrimitive ("CELL?", 1)]
        public static bool IsCell (out object answer, object arg)
        {
            answer = arg is Cell;
            return false;
        }
    }
}
