using System;
using System.Diagnostics;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    sealed class Record: SchemeObject, ISystemPair
    {
        object [] slots;

        public Record (object [] slots)
            : base (TC.RECORD)
        {
            this.slots = (object []) slots.Clone ();
        }

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

        [SchemePrimitive ("%RECORD", -1)]
        public static object PrimitiveRecord (Interpreter interpreter, object [] arglist)
        {
            return interpreter.Return (new Record (arglist));
        }

        [SchemePrimitive ("%RECORD?", 1)]
        public static object IsRecord (Interpreter interpreter, object arg0)
        {
            return interpreter.Return (arg0 is Record);
        }

        [SchemePrimitive ("%RECORD-REF", 2)]
        public static object RecordRef (Interpreter interpreter, object record, object idx)
        {
            return interpreter.Return (((Record) record).Ref ((int) idx));
        }

        [SchemePrimitive ("%RECORD-SET!", 3)]
        public static object RecordSet (Interpreter interpreter, object record, object idx, object value)
        {
            return interpreter.Return (((Record) record).Set ((int) idx, value));
        }

        #region ISystemPair Members

        public object SystemPairCar
        {
            get
            {
                return Ref(0);
            }
            set
            {
                throw new NotImplementedException();
            }
        }

        public object SystemPairCdr
        {
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
    }
}
