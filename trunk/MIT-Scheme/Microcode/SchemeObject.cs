
using System;

namespace Microcode
{
    // Root of any object that isn't just lifted from
    // the underlying runtime.
    [Serializable]
    abstract public class SchemeObject
    {
        abstract public TC TypeCode
        {
            get;
        }
    }
}
