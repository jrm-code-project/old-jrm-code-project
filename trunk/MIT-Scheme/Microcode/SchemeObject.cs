using System;

namespace Microcode
{
    // Root of any object that isn't just lifted from
    // the underlying runtime.
    // Must be public.
    [Serializable]
    public abstract class SchemeObject
    {
        abstract public TC TypeCode
        {
            get;
        }
    }
}
