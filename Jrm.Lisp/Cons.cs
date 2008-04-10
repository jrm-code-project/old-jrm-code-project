using System;
using System.Diagnostics;
using System.Collections;
using System.Collections.Generic;

namespace Lisp
{
    [CLSCompliant(true)]
    public struct ConsEnumerator : IEnumerator
    {
        readonly Cons head;
        Cons current;
        bool exhausted;

        public ConsEnumerator (Cons head)
        {
            this.head = head;
            this.current = null;
            this.exhausted = false;
        }

        public override bool Equals (object obj)
        {
            if (obj == null) return false;
            if (obj.GetType () != typeof (ConsEnumerator))
                return false;

            ConsEnumerator that = (ConsEnumerator) obj;
            return this.head == that.head
                && this.current == that.current
                && this.exhausted == that.exhausted;
        }

        public override int GetHashCode ()
        {
            return this.head.GetHashCode ();
        }

        public static bool operator== (ConsEnumerator left, ConsEnumerator right)
        {
            return left.head == right.head
                && left.current == right.current
                && left.exhausted == right.exhausted;
        }

        public static bool operator!= (ConsEnumerator left, ConsEnumerator right)
        {
            return !(left == right);
        }

        public bool MoveNext ()
        {
            if (current == null) {
                if (exhausted)
                    throw new NotImplementedException ();
                current = head;
                return true;
            }
            else {
                current = (Cons) current.Cdr;
                if (current == null)
                    exhausted = true;
                return !exhausted;
            }
        }

        public void Reset () {
            current = null;
            exhausted = false;
        }

        public object Current
        {
            get
            {
               return current.Car;
            }
        }
    }

    [CLSCompliant(true)]
    public sealed class Cons : ReadOnlyCollectionBase, ICollection, IEnumerable, IList
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly object car;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly object cdr;

        public Cons (object car, object cdr)
        {
            this.car = car;
            this.cdr = cdr;
        }

        public object Car
        {
            [DebuggerStepThrough]
            get
            {
                return this.car;
            }
        }

        public object Cdr
        {
            [DebuggerStepThrough]
            get
            {
                return this.cdr;
            }
        }

        public void CopyTo (Array array)
        {
            Utility.Ignore (this);
            Utility.Ignore(array);
            throw new NotImplementedException ("CopyTo");
        }

        public void CopyTo (Array array, int arrayIndex)
        {
            Utility.Ignore (this);
            Utility.Ignore(array);
            Utility.Ignore(arrayIndex);
            throw new NotImplementedException ("CopyTo");
        }

        public void CopyTo (int index, Array array, int arrayIndex, int count)
        {
            Utility.Ignore (this);
            Utility.Ignore(index);
            Utility.Ignore(array);
            Utility.Ignore(arrayIndex);
            Utility.Ignore(count);
            throw new NotImplementedException ("CopyTo");
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public bool IsSynchronized
        {
            get
            {
                Utility.Ignore (this);
                throw new NotImplementedException("IsSynchronized");
            }
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SyncRoot
        {
            get
            {
                return this.car;
            }
        }

        public static Cons SubvectorToList (object [] vector, int start, int limit)
        {
            if (vector == null) {
                if (start == limit)
                    return null;
                throw new ArgumentNullException ("vector");
            }
              
            Cons answer = null;
            int count = 1;
            for (int i = start; i < limit; i++) {
                answer = new Cons (vector [limit - count], answer);
                count += 1;
            }
            return answer;
        }

        public static Cons VectorToList (object [] vector)
        {
            return vector == null ? null : SubvectorToList (vector, 0, vector.Length);
        }

        #region IList Members

        int IList.Add (object value)
        {
            throw new NotImplementedException ();
        }

        void IList.Clear ()
        {
            throw new NotImplementedException ();
        }

        bool IList.Contains (object value)
        {
            throw new NotImplementedException ();
        }

        int IList.IndexOf (object value)
        {
            throw new NotImplementedException ();
        }

        void IList.Insert (int index, object value)
        {
            throw new NotImplementedException ();
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        bool IList.IsFixedSize
        {
            get
            {
                return true;
            }
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        bool IList.IsReadOnly
        {
            get
            {
                return true;
            }
        }

        void IList.Remove (object value)
        {
            throw new NotImplementedException ();
        }

        void IList.RemoveAt (int index)
        {
            throw new NotImplementedException ();
        }

        object IList.this [int index]
        {
            get
            {
                throw new NotImplementedException ();
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        #endregion

        #region ICollection Members

        void ICollection.CopyTo (Array array, int index)
        {
            Utility.Ignore(array);
            Utility.Ignore(index);
            throw new NotImplementedException ();
        }
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        int ICollection.Count
        {
            get
            {
                int length = 1;
                Cons tail = this;
                while (true) {
                    Cons next = tail.cdr as Cons;
                    if (next == null) {
                        if (tail.cdr == null)
                            break;
                        else
                            throw new NotImplementedException ("dotted list");
                    }
                    length += 1;
                    tail = next;
                }
                return length;
            }
        }
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        bool ICollection.IsSynchronized
        {
            get
            {
                throw new NotImplementedException ();
            }
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        object ICollection.SyncRoot
        {
            get
            {
                throw new NotImplementedException ();
            }
        }

        #endregion

        #region IEnumerable Members

        IEnumerator IEnumerable.GetEnumerator ()
        {
            return new ConsEnumerator (this);
        }

        #endregion
    }

    public struct ConsListEnumerator<T> : IEnumerator<T>
    {
        readonly ConsCollection<T> head;
        ConsCollection<T> current;
        bool exhausted;

        public ConsListEnumerator (ConsCollection<T> head)
        {
            this.head = head;
            this.current = null;
            this.exhausted = false;
        }

        public override bool Equals (object obj)
        {
            if (obj == null) return false;
            if (obj.GetType () != typeof (ConsListEnumerator<T>))
                return false;

            ConsListEnumerator<T> that = (ConsListEnumerator<T>) obj;
            return this.head == that.head
                && this.current == that.current
                && this.exhausted == that.exhausted;
        }

        public override int GetHashCode ()
        {
            return this.head.GetHashCode ();
        }

        public static bool operator == (ConsListEnumerator<T> left, ConsListEnumerator<T> right)
        {
            return left.head == right.head
                && left.current == right.current
                && left.exhausted == right.exhausted;
        }

        public static bool operator != (ConsListEnumerator<T> left, ConsListEnumerator<T> right)
        {
            return !(left == right);
        }


        public void Dispose () {
        }

        public bool MoveNext ()
        {
            if (current == null) {
                if (exhausted)
                    throw new NotImplementedException ();
                current = head;
                return true;
            }
            else {
                current = current.Cdr;
                if (current == null)
                    exhausted = true;
                return !exhausted;
            }
        }

        public void Reset ()
        {
            current = null;
            exhausted = false;
        }

        public T Current
        {
            get
            {
                return current.Car;
            }
        }

        object IEnumerator.Current
        {
            get
            {
                return current.Car;
            }
        }
    }

    // A linked list of cons cells.
    public sealed class ConsCollection<T> : IList<T>, IList, ICollection<T>, ICollection, IEnumerable<T>, IEnumerable
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly T car;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        readonly ConsCollection<T> cdr;

        public ConsCollection (T car, ConsCollection<T> cdr)
        {
            this.car = car;
            this.cdr = cdr;
        }

        public ConsCollection (T [] vector, int start, int limit)
        {
            if (vector == null) throw new ArgumentNullException ("vector");
            if (start >= limit) throw new NotImplementedException ("bogus arguments");
            this.car = vector [start];
            if (start + 1 == limit)
                this.cdr = null;
            else
                this.cdr = new ConsCollection<T> (vector, start + 1, limit);
        }

        public T Car
        {
            [DebuggerStepThrough]
            get
            {
                return this.car;
            }
        }

        public ConsCollection<T> Cdr
        {
            [DebuggerStepThrough]
            get
            {
                return this.cdr;
            }
        }

        //public static ConsCollection<T> SubvectorToList (T [] vector, int start, int limit)
        //{
        //    if (vector == null) {
        //        if (start == limit)
        //            return null;
        //        throw new ArgumentNullException ("vector");
        //    }

        //    ConsCollection<T> answer = null;
        //    int count = 1;
        //    for (int i = start; i < limit; i++) {
        //        answer = new ConsCollection<T> (vector [limit - count], answer);
        //        count += 1;
        //    }
        //    return answer;
        //}

        //public static ConsCollection<T> VectorToList (T [] vector)
        //{
        //    return SubvectorToList (vector, 0, vector.Length);
        //}


        void ICollection<T>.Add (T thing)
        {
            throw new NotSupportedException ();
        }

        void ICollection<T>.Clear ()
        {
            throw new NotSupportedException ();
        }

        bool ICollection<T>.Contains (T thing)
        {
            return object.ReferenceEquals (this.car,thing)
                 || (this.Cdr != null 
                     && ((ICollection<T>) this.Cdr).Contains (thing));
        }

        void ICollection.CopyTo (Array array, int index)
        {
            Utility.Ignore(array);
	    Utility.Ignore(index);
            throw new NotImplementedException ("CopyTo");
        }

        void ICollection<T>.CopyTo (T[] array, int index)
        {
            Utility.Ignore(array);
	    Utility.Ignore(index);
            throw new NotImplementedException ("CopyTo");
        }

        bool ICollection<T>.Remove (T thing)
        {
            Utility.Ignore(thing);
            throw new NotSupportedException ();
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        int ICollection<T>.Count
        {
            get
            {
                int length = 1;
                ConsCollection<T> tail = this;
                while (true) {
                    ConsCollection<T> next = tail.cdr;
                    if (next == null) {
                        if (tail.cdr == null)
                            break;
                        else
                            throw new NotImplementedException ("dotted list");
                    }
                    length += 1;
                    tail = next;
                }
                return length;
            }
        }
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        int ICollection.Count
        {
            get
            {
                int length = 1;
                ConsCollection<T> tail = this;
                while (true) {
                    ConsCollection<T> next = tail.cdr;
                    if (next == null) {
                        if (tail.cdr == null)
                            break;
                        else
                            throw new NotImplementedException ("dotted list");
                    }
                    length += 1;
                    tail = next;
                }
                return length;
            }
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        bool ICollection<T>.IsReadOnly 
        {
            get
            {
                return true;
            }
        }

        public IEnumerator<T> GetEnumerator()
        {
            return new ConsListEnumerator<T> (this);
        }

        IEnumerator IEnumerable.GetEnumerator ()
        {
            return (IEnumerator) new ConsListEnumerator<T> (this);
        }

        IEnumerator<T> IEnumerable<T>.GetEnumerator ()
        {
            return new ConsListEnumerator<T> (this);
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public bool IsSynchronized
        {
            get
            {
                return false;
            }
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        public object SyncRoot
        {
            get
            {
                return this.car;
            }
        }

        #region IList<T> Members

        int IList<T>.IndexOf (T item)
        {
            throw new NotImplementedException ();
        }

        void IList<T>.Insert (int index, T item)
        {
            throw new NotImplementedException ();
        }

        void IList<T>.RemoveAt (int index)
        {
            throw new NotImplementedException ();
        }

        T IList<T>.this [int index]
        {
            get
            {
                throw new NotImplementedException ();
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        #endregion

        #region IList Members

        int IList.Add (object value)
        {
            throw new NotImplementedException ();
        }

        void IList.Clear ()
        {
            throw new NotImplementedException ();
        }

        bool IList.Contains (object value)
        {
            throw new NotImplementedException ();
        }

        int IList.IndexOf (object value)
        {
            throw new NotImplementedException ();
        }

        void IList.Insert (int index, object value)
        {
            throw new NotImplementedException ();
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]

        bool IList.IsFixedSize
        {
            get { return true; }
        }

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]

        bool IList.IsReadOnly
        {
            get { return true; }
        }

        void IList.Remove (object value)
        {
            throw new NotImplementedException ();
        }

        void IList.RemoveAt (int index)
        {
            throw new NotImplementedException ();
        }

        object IList.this [int index]
        {
            get
            {
                throw new NotImplementedException ();
            }
            set
            {
                throw new NotImplementedException ();
            }
        }

        #endregion

        #region ICollection Members

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]

        bool ICollection.IsSynchronized
        {
            get { throw new NotImplementedException (); }
        }
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]

        object ICollection.SyncRoot
        {
            get { throw new NotImplementedException (); }
        }

        #endregion
    }
}
