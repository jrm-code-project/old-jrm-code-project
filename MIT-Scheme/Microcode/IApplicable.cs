using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    interface IApplicable
    {
        bool Apply (out object answer, ref SCode expression, ref Environment environment, object [] args);
        bool Call (out object anwswer, ref SCode expression, ref Environment environment);
        bool Call (out object answer, ref SCode expression, ref Environment environment, object arg0);
        bool Call (out object answer, ref SCode expression, ref Environment environment, object arg0, object arg1);
    }
}
