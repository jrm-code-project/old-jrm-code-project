using System;
using System.Diagnostics;

namespace Microcode
{
    sealed class Constant
    {
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        static object sharpT = true;
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        static Constant defaultObject;
        [DebuggerBrowsable(DebuggerBrowsableState.Never)]
        static Constant eofObject;
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        static Constant aux;
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        static Constant key;
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        static Constant optional;
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        static Constant rest;
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        static Constant externalUnassigned;
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        static Constant unspecific;
        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        static object sharpF = false;

        [DebuggerBrowsable (DebuggerBrowsableState.Never)]
        string name;

        private Constant (string name)
        {
            this.name = name;
        }

        public override string ToString ()
        {
            return "#!" + this.name;
        }

        internal static object Decode (uint code)
        {
            switch (code)
            {
                case 0:
                    return sharpT;
                case 1:
                    return Unspecific;
                case 2:
                    return ExternalUnassigned;
                case 3:
                    return LambdaOptionalTag;
                case 4:
                    return LambdaRestTag;
                case 5:
                    return LambdaKeyTag;
                case 6:
                    return EofObject;
                case 7:
                    return DefaultObject;
                case 8:
                    return LambdaAuxTag;
                case 9:
                    return null;
                default:
                    throw new NotImplementedException ();
            }
        }

        public static Constant DefaultObject
        {
            [DebuggerStepThrough]
            get
            {
                if (defaultObject == null)
                    defaultObject = new Constant ("default");
                return defaultObject;
            }
        }

        public static Constant EofObject
        {
            [DebuggerStepThrough]
            get
            {
                if (eofObject == null)
                    eofObject = new Constant("eof");
                return eofObject;
            }
        }

        public static Constant LambdaAuxTag
        {
            [DebuggerStepThrough]
            get
            {
                if (aux == null)
                    aux = new Constant ("aux");
                return aux;
            }
        }

        public static Constant LambdaKeyTag
        {
            [DebuggerStepThrough]
            get
            {
                if (key == null)
                    key = new Constant ("key");
                return key;
            }
        }


        public static Constant LambdaOptionalTag
        {
            [DebuggerStepThrough]
            get
            {
                if (optional == null)
                    optional = new Constant ("optional");
                return optional;
            }
        }

        public static Constant LambdaRestTag
        {
            [DebuggerStepThrough]
            get
            {
                if (rest == null)
                    rest = new Constant ("rest");
                return rest;
            }
        }

        public static Constant ExternalUnassigned
        {
            [DebuggerStepThrough]
            get
            {
                if (externalUnassigned == null)
                    externalUnassigned = new Constant ("Unassigned");
                return externalUnassigned;
            }
        }

        public static Constant Unspecific
        {
            [DebuggerStepThrough]
            get
            {
                if (unspecific == null)
                    unspecific = new Constant ("Unspecific");
                return unspecific;
            }
        }
   }
}
