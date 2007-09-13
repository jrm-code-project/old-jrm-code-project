using System;
using System.Runtime.InteropServices;
using System.Runtime.InteropServices.ComTypes;

namespace LScript
{
    class TypeInfoMarshaler: ICustomMarshaler
    {
        static TypeInfoMarshaler theMarshaler;

        public TypeInfoMarshaler ()
        {
        }

        public object MarshalNativeToManaged (IntPtr pNativeData)
        {
           return Marshal.GetTypedObjectForIUnknown (pNativeData, typeof (ITypeInfo));
        }

        public IntPtr MarshalManagedToNative (object managedObj)
        {
            System.Diagnostics.Debug.Assert (false, "MarshalManagedToNative()");
            return IntPtr.Zero;
        }

        public void CleanUpNativeData (IntPtr pNativeData)
        {
            Marshal.Release (pNativeData);
        }

        public void CleanUpManagedData (object managedObj)
        {
        }

        public int GetNativeDataSize ()
        {
            System.Diagnostics.Debug.Assert (false, "GetNativeDataSize()");
            return -1;
        }

        public static ICustomMarshaler GetInstance (string cookie)
        {
            if (theMarshaler == null)
                theMarshaler = new TypeInfoMarshaler ();
            return theMarshaler;
        }
    }
}
