using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Lisp
{
    interface ILocation<T>
    {
        T Value
        {
            get;
            set;
        }
    }
}
