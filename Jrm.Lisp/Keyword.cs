using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Lisp
{
 
        [CLSCompliant (true)]
        [System.Diagnostics.DebuggerDisplay (":{Name,nq}")]
        public class Keyword : Symbol
        {
            public Keyword (string name)
                : base (name, Package.Keyword)
            {
            }

            public override string ToString ()
            {
                return ":" + this.Name;
            }
        }

        public abstract class KeywordArgumentBase
        {
            readonly Symbol key;
            private bool IsAssigned;

            protected KeywordArgumentBase (Symbol key)
            {
                this.key = key;
            }

            public object Key
            {
                get
                {
                    return this.key;
                }
            }

            public bool Supplied
            {
                get
                {
                    return this.IsAssigned;
                }
                set
                {
                    this.IsAssigned = value;
                }
            }

            public abstract void Assign (object value);

            public static bool ProcessKeywordArguments (KeywordArgumentBase [] keywordArguments,
                                                        object [] parameters, bool allowOtherKeys)
            {
                if (keywordArguments == null)
                    throw new ArgumentNullException ("keywordArguments");
                if (parameters == null)
                    throw new ArgumentNullException ("parameters");

                bool otherKeysSeen = false;

                for (int i = 0; i < parameters.Length; i += 2) {
                    Symbol key = (Symbol) parameters [i];
                    bool assigned = false;
                    foreach (KeywordArgumentBase keyarg in keywordArguments) {
                        if (keyarg.Key == key) {
                            keyarg.Assign (parameters [i + 1]);
                            assigned = true;
                            break;
                        }
                    }
                    if (key == KW.AllowOtherKeys)
                        allowOtherKeys = (bool) parameters [i + 1];
                    else if (!assigned)
                        otherKeysSeen = true;
                }
                if (otherKeysSeen && !allowOtherKeys)
                    throw new NotImplementedException ();
                return otherKeysSeen;
            }
        }

        public class KeywordArgument<T> : KeywordArgumentBase
        {
            T value;

            public KeywordArgument (Symbol key) : base (key)
            {
            }

            public T Value
            {
                get
                {
                    return this.value;
                }

                set
                {
                    this.value = value;
                }
            }

            public override void Assign (object value)
            {
                if (!this.Supplied) {
                    this.value = (T) value;
                    this.Supplied = true;
                }
            }
        }

    
}
