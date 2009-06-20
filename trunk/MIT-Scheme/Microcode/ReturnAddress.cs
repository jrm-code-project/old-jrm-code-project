using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    [Serializable]
    sealed class ReturnAddress : SchemeObject
    {
        ReturnCode code;

        public ReturnAddress (ReturnCode code)
            : base (TC.FIXNUM)
        {
            this.code = code;
        }

        public ReturnCode Code
        {
            get
            {
                return this.code;
            }
        }

        [SchemePrimitive ("RETURN-CODE?", 1, true)]
        public static bool IsReturnCode (out object answer, object arg)
        {
            answer = arg is ReturnCode;
            return false;
        }

        [SchemePrimitive ("MAP-MACHINE-ADDRESS-TO-CODE", 2, false)]
        public static bool MapMachineAddressToCode (out object answer, object arg0, object arg1)
        {
            TC type = (TC) arg0;
            switch (type) {
                case TC.RETURN_CODE:
                    answer = (int) (ReturnCode) arg1;
                    break;

                default:
                    throw new NotImplementedException ();
            }
            return false;
        }

        [SchemePrimitive ("MAP-CODE-TO-MACHINE-ADDRESS", 2, false)]
        public static bool MapCodeToMachineAddress (out object answer, object arg0, object arg1)
        {
            TC type = (TC) arg0;
            switch (type) {
                case TC.RETURN_CODE:
                    answer = new ReturnAddress ((ReturnCode) arg1);
                    break;
                default:
                    throw new NotImplementedException ();
            }
            return false;
        }
    }
}
