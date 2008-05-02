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
        public static object MapMachineAddressToCode (Interpreter interpreter, object arg0, object arg1)
        {
            TC type = (TC) arg0;
            switch (type)
            {
                case TC.RETURN_CODE:
                    return interpreter.Return ((int) (ReturnCode) arg1);
                default:
                    throw new NotImplementedException ();
            }
        }

        [SchemePrimitive ("MAP-CODE-TO-MACHINE-ADDRESS", 2)]
        public static object MapCodeToMachineAddress (Interpreter interpreter, object arg0, object arg1)
        {
            TC type = (TC) arg0;
            switch (type)
            {
                case TC.RETURN_CODE:
                    return interpreter.Return (new ReturnAddress ((ReturnCode) arg1));
                default:
                    throw new NotImplementedException ();
            }
        }


    }
}
