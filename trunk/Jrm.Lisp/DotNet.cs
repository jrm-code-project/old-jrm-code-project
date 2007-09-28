using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Lisp
{
    public sealed class DotNet
    {
        private DotNet ()
        {
        }

        private static bool IsInitialized;

        public static bool Enable ()
        {
            if (IsInitialized) {
                return false;
            }
            else {
                IsInitialized = true;
                return true;
            }
        }

       
    }
}
