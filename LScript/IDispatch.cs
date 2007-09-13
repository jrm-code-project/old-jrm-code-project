using System;
using System.Runtime.InteropServices;
using System.Runtime.InteropServices.ComTypes;

namespace LScript
{
    [ComImport,
     Guid ("00020400-0000-0000-C000-000000000046"),
     InterfaceType (ComInterfaceType.InterfaceIsIUnknown)]
    public interface IDispatch
    {
        [PreserveSig, Obsolete ("PLACEHOLDER", true)]
        int GetTypeInfoCount ();

        [return: MarshalAs (UnmanagedType.CustomMarshaler, MarshalTypeRef = typeof (TypeInfoMarshaler))]
        ITypeInfo GetTypeInfo (int iTInfo, int lcid);

        [PreserveSig, Obsolete ("PLACEHOLDER", true)]
        int GetIDsOfNames ();

        [PreserveSig, Obsolete ("PLACEHOLDER", true)]
        int Invoke ();
    }
}
