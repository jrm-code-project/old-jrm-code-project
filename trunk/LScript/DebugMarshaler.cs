using System;
using System.Runtime.InteropServices;

namespace LScript
{
    // This class is useful for figuring out the right way to marshal
    // things from hand-rolled interop assemblies.
    public class DebugMarshaler : ICustomMarshaler
    {
        string cookie;

        public DebugMarshaler (string cookie)
        {
            this.cookie = cookie;
        }

        public object MarshalNativeToManaged (IntPtr pNativeData)
        {
            System.Diagnostics.Debug.Assert (false, "MarshalNativeToManaged(\""+cookie+"\","+pNativeData.ToString()+")");
            return null;
        }

        public IntPtr MarshalManagedToNative (object managedObj)
        {
            System.Diagnostics.Debug.Assert (false, "MarshalManagedToNative()");
            return IntPtr.Zero;
        }

        public void CleanUpNativeData (IntPtr pNativeData)
        {
            System.Diagnostics.Debug.Assert (false, "CleanUpNativeData()");
            Marshal.Release (pNativeData);
        }

        public void CleanUpManagedData (object managedObj)
        {
            System.Diagnostics.Debug.Assert (false, "CleanUpManagedData()");
        }

        public int GetNativeDataSize ()
        {
            System.Diagnostics.Debug.Assert (false, "GetNativeDataSize()");
            return -1;
        }

        public static ICustomMarshaler GetInstance (string cookie)
        {
            System.Diagnostics.Debug.Assert (false, "GetInstance(\""+cookie+"\")");
                return  new DebugMarshaler (cookie);
        }
    }
}
