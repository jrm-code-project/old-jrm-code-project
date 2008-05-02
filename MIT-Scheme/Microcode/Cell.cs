using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    class Cell
    {
        [SchemePrimitive ("CELL?", 1)]
        public static object IsCell (Interpreter interpreter, object arg)
        {
            return interpreter.Return (arg is Cell);
        }
    }
}
