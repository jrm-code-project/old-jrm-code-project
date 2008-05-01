using System;

namespace Microcode
{
    sealed class NonMarkedVector
    {
        public object contents;

        public NonMarkedVector (object contents)
        {
            this.contents = contents;
        }
    }
}
