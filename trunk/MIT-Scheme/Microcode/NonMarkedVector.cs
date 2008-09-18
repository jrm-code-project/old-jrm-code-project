using System;

namespace Microcode
{
    [Serializable]
    sealed class NonMarkedVector
    {
        public object contents;

        public NonMarkedVector (object contents)
        {
            this.contents = contents;
        }

        [SchemePrimitive ("MANIFEST-NM-VECTOR?", 1, true)]
        static public bool IsManifestNMVector (out object answer, object arg)
        {
            answer = false;
            return false;
        }
    }
}
