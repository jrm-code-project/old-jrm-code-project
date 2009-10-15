using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    interface ITopLevelEnvironment
    {
        Dictionary<Symbol, ValueCell> TopLevelVariables { get; }
        TRet LocateVariable<TRet> (object name, Func<GlobalEnvironment, TRet> ifGlobal, Func<TRet> ifNotFound);
    }
}
