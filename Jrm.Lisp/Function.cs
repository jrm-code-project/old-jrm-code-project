using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Lisp
{
    public delegate object Applicable (params object [] arguments);

    public delegate object Function2 (object arg0, object arg1);
    public delegate object Function3 (object arg0, object arg1, object arg2);

    public delegate bool Predicate (object argument);
}
