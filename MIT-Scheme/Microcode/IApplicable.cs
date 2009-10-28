using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    interface IApplicable
    {
        bool Apply (out object answer, ref Control expression, ref Environment environment, object [] args);
        // Why so many variations?  Because these are markedly more popular
        // than anything longer.
        bool Call (out object answer, ref Control expression, ref Environment environment);
        bool Call (out object answer, ref Control expression, ref Environment environment, object arg0);
        bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1);
        bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2);
        bool Call (out object answer, ref Control expression, ref Environment environment, object arg0, object arg1, object arg2, object arg3);
    }
}
