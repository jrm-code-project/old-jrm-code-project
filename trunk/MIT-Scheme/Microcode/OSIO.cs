using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;

namespace Microcode
{
    static class OSIO
    {
        public const int SelectModeRead = 1;
        public const int SelectModeWrite = 2;
        public const int SelectModeError = 4;
        public const int SelectModeHup = 8;
        public const int SelectInterrupt = -1;
        public const int SelectProcessStatusChange = -2;
    }
}
