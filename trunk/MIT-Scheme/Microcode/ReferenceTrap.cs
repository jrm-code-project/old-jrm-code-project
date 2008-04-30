
namespace Microcode
{
    class ReferenceTrap
    {
        static ReferenceTrap rtzero;
        object innerObject;

        ReferenceTrap (object innerObject)
        {
            this.innerObject = innerObject;
        }

        public static ReferenceTrap Make (object innerObject)
        {
            if ((innerObject is int) && ((int) innerObject == 0))
            {
                if (rtzero == null)
                    rtzero = new ReferenceTrap (0);
                return rtzero;
            }
            return new ReferenceTrap (innerObject);
        }
    }
}
