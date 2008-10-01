using System;
using System.Diagnostics;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    [Serializable]
    sealed class Record: SchemeObject, ISystemPair
    {
        static int [] histogram = new int [128];

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly object [] slots;

        public Record (object [] slots)
            : base (TC.RECORD)
        {
            this.slots = slots;
        }

        public object [] Slots
        {
            [DebuggerStepThrough]
            get
            {
                return this.slots;
            }
        }

        #region ISystemPair Members

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemPairCar
        {
            [DebuggerStepThrough]
            get
            {
                return Ref(0);
            }
            set
            {
                throw new NotImplementedException();
            }
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SystemPairCdr
        {
            [DebuggerStepThrough]
            get
            {
                return this;
            }
            set
            {
                throw new NotImplementedException();
            }
        }

        #endregion

        object this [int slot]
        {
            [DebuggerStepThrough]
            get {
                return this.slots [slot];
                }
        }      

        public object Ref (int slot)
        {
            return this.slots [slot];
        }

        public object Set (int slot, object value)
        {
            object old = this.slots [slot];
            this.slots [slot] = value;
            return old;
        }

        [SchemePrimitive ("%RECORD", -1, true)]
        public static bool PrimitiveRecord (out object answer, object [] arglist)
        {
            answer = new Record (arglist);
            return false;
        }

        [SchemePrimitive ("%RECORD?", 1, true)]
        public static bool IsRecord (out object answer, object arg0)
        {
            answer = arg0 is Record;
            return false;
        }

        [SchemePrimitive ("%RECORD-REF", 2, false)]
        public static bool RecordRef (out object answer, object record, object idx)
        {
            histogram [(int) idx] += 1;
            answer = ((Record) record).Ref ((int) idx);
            return false;
        }

        [SchemePrimitive ("%RECORD-REF0", 1, false)]
        public static bool RecordRef0 (out object answer, object record)
        {
            histogram [0] += 1;
            answer = ((Record) record).Ref (0);
            return false;
        }

        [SchemePrimitive ("%RECORD-REF1", 1, false)]
        public static bool RecordRef1 (out object answer, object record)
        {
            histogram [1] += 1;
            answer = ((Record) record).Ref (1);
            return false;
        }

        [SchemePrimitive ("%RECORD-REF3", 1, false)]
        public static bool RecordRef3 (out object answer, object record)
        {
            histogram [3] += 1;
            answer = ((Record) record).Ref (3);
            return false;
        }

        [SchemePrimitive ("%RECORD-SET!", 3, false)]
        public static bool RecordSet (out object answer, object record, object idx, object value)
        {
            //if ((int)idx == 3)
            //    Debug.WriteLine ("slot 3");
            answer = ((Record) record).Set ((int) idx, value);
            return false;
        }

    }
}
