using System;
using System.Runtime.Serialization;

namespace Lisp
{
    [Serializable]
    public class ImplementationBugException : Exception
    {
        public ImplementationBugException ()
            : base ()
        {
        }

        public ImplementationBugException (string message)
            : base (message)
        {
        }

        public ImplementationBugException (string message, Exception innerException)
            : base (message, innerException)
        {
        }

        protected ImplementationBugException (SerializationInfo info, StreamingContext context)
            : base(info, context)
      {
      }
    }
}
