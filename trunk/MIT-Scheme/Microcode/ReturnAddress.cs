using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{

    class ReturnAddress
    {
        ReturnCode code;

        public ReturnAddress (ReturnCode code)
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

        [SchemePrimitive ("MAP-MACHINE-ADDRESS-TO-CODE", 2)]
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

        [SchemePrimitive ("MAP-CODE-TO-MACHINE-ADDRESS", 2)]
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
