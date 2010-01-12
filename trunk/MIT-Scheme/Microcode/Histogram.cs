using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Text;

namespace Microcode
{
    sealed class Histogram<T>
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        private object lockObject = new Object ();

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        Dictionary<T, long> entries = new Dictionary<T, long> ();

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        bool isNoisy;

        public Histogram ()
        {
        }

        public Histogram (bool isNoisy)
        {
            this.isNoisy = true;
        }

        [DebuggerStepThrough]
        public void Note (T item)
        {
            lock (this.lockObject) {
                if (entries.ContainsKey (item))
                    entries [item] += 1;
                else {
                    if (this.isNoisy)
                        Debug.WriteLine ("Histogram: first entry for " + item.ToString ());
                    entries [item] = 1;
                }
            }
        }

        public void Clear ()
        {
            lock (this.lockObject) {
                entries.Clear ();
            }
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
