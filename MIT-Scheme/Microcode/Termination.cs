using System;
using System.Diagnostics;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    public class Termination
    {
        string message;

        static Termination unexpectedExit;

        Termination (string message)
        {
            this.message = message;
        }

        public string Message
        {
            [DebuggerStepThrough]
            get
            {
                return this.message;
            }
        }

        static public Termination UNEXPECTED_EXIT
        {
            get
            {
                if (unexpectedExit == null)
                    unexpectedExit = new Termination ("Unexpected exit from the interpreter!");
                return unexpectedExit;
            }
        }
    }
}
