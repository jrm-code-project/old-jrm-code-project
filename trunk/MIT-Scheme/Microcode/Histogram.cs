using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;

namespace Microcode
{
    sealed public class Histogram<T>
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        Dictionary<T, long> entries = new Dictionary<T, long> ();

        [DebuggerStepThrough]
        public void Note (T item)
        {
            if (entries.ContainsKey (item))
                entries [item] += 1;
            else
                entries [item] = 1;
        }

        static public long GetKey (KeyValuePair<T, long> entry) { return entry.Value; }

        public KeyValuePair<T, long> [] Entries
        {
            get
            {
                return this.entries.OrderByDescending<KeyValuePair<T, long>, long> (Histogram<T>.GetKey).ToArray ();
            }
        }
    }
}
