using System;
using System.Runtime.InteropServices;
using System.Runtime.InteropServices.ComTypes;

namespace LScript
{
    // A hand-rolled interop assembly for active scripting.
    [ComVisible(true), Flags]
    public enum ScriptInfo : uint
    {
        None = 0x0000,

        IUnknown = 0x0001,
        ITypeInfo = 0x0002,
    }

    [ComVisible (true), Flags]
    public enum ScriptItem : uint
    {
        None = 0x0000,

        IsVisible = 0x0002,
        IsSource = 0x0004,
        GlobalMembers = 0x0008,
        IsPersistent = 0x0040,
        CodeOnly = 0x0200,
        NoCode = 0x0400,
    }

    [ComVisible (true)]
    public enum ScriptState : uint
    {
        Uninitialized = 0,
        Started = 1,
        Connected = 2,
        Disconnected = 3,
        Closed = 4,
        Initialized = 5,
    }

    [ComVisible (true), Flags]
    public enum ScriptText : uint
    {
        None = 0x0000,

        DelayExecution = 0x0001,
        IsVisible = 0x0002,
        IsExpression = 0x0020,
        IsPersistent = 0x0040,
        HostManageSource = 0x0080,
    }

    [ComVisible (true)]
    public enum ScriptThreadState : uint
    {
        NotInScript = 0,
        Running = 1,
    }

    [ComImport,
     ComVisible (true),
     InterfaceType (ComInterfaceType.InterfaceIsIUnknown),
     Guid ("BB1A2AE1-A4F9-11cf-8F20-00805F2CD064")]
    public interface IActiveScript
    {
        void SetScriptSite ([MarshalAs (UnmanagedType.Interface)] IActiveScriptSite site);
        void GetScriptSite (ref System.Guid riid, out System.IntPtr ppvObject);
        void SetScriptState (ScriptState ss);
        void GetScriptState (out ScriptState ss);
        void Close ();
        void AddNamedItem ([MarshalAs (UnmanagedType.LPWStr)] string pstrName, ScriptItem dwFlags);
        void AddTypeLib (ref System.Guid rguidTypeLib, uint dwMajor, uint dwMinor, uint dwFlags);
        void GetScriptDispatch (string pstrItemName, out object ppdisp);
        void GetCurrentScriptThreadID (out uint id);
        void GetScriptThreadID (uint threadid, out uint id);
        void GetScriptThreadState (uint id, out ScriptThreadState state);
        // This next one is wrong, but I can't figure out how to fix it yet.
        void InterruptScriptThread (uint id, IntPtr info, uint flags);
        void Clone (out IActiveScript item);
    };

    [ComImport,
     ComVisible (true),
     Guid ("BB1A2AE2-A4F9-11cf-8F20-00805F2CD064"),
     InterfaceType (ComInterfaceType.InterfaceIsIUnknown)]
    public interface IActiveScriptParse
    {
        void InitNew ();
        void AddScriptlet ([MarshalAs (UnmanagedType.LPWStr)]string defaultName,
                    [MarshalAs (UnmanagedType.LPWStr)]string code,
                    [MarshalAs (UnmanagedType.LPWStr)]string itemName,
                    [MarshalAs (UnmanagedType.LPWStr)]string subItemName,
                    [MarshalAs (UnmanagedType.LPWStr)]string eventName,
                    [MarshalAs (UnmanagedType.LPWStr)]string delimiter,
                    uint sourceContextCookie,
                    uint startingLineNumber,
                    uint flags,
                  // These next two are wrong, but I can't figure out how to fix them yet.
	            IntPtr result,
                    IntPtr info);

        void ParseScriptText (
            [MarshalAs (UnmanagedType.LPWStr)] string code,
            [MarshalAs (UnmanagedType.LPWStr)] string itemName,
            [MarshalAs (UnmanagedType.IUnknown)] object context,
            [MarshalAs (UnmanagedType.LPWStr)] string delimiter,
            int sourceContextCookie,
            uint startingLineNumber,
            ScriptText flags,
            // These next two are wrong, but I can't figure out how to fix them yet.
            IntPtr result,
            IntPtr info);
    }

 
    [ComImport,
     Guid ("DB01A1E3-A42B-11cf-8F20-00805F2CD064"),
     InterfaceType (ComInterfaceType.InterfaceIsIUnknown)]
    public interface IActiveScriptSite
    {
        void GetLCID (out uint id);
        void GetItemInfo ([MarshalAs (UnmanagedType.LPWStr)] string pstrName,
                          ScriptInfo dwReturnMask,
                          [Out, MarshalAs (UnmanagedType.IDispatch)] out object item,
                          [Out, MarshalAs (UnmanagedType.IUnknown)] out object ppti);
        void GetDocVersionString (out string v);
        void OnScriptTerminate (ref object result, ref System.Runtime.InteropServices.ComTypes.EXCEPINFO info);
        void OnStateChange (ScriptState state);
        void OnScriptError ([MarshalAs (UnmanagedType.IUnknown)] object err);
        void OnEnterScript ();
        void OnLeaveScript ();
    }
}
