using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;

using System.Linq;
using System.Text;

namespace LScriptTest
{

    [ComImport, Guid ("62416980-8B84-4c51-A09C-AB9623C3CC3E")]
    public class LScript1
    {
    }
    //
    // Scripting Interfaces
    //

    public enum ScriptState : uint
    {
        Uninitialized = 0,
        Started = 1,
        Connected = 2,
        Disconnected = 3,
        Closed = 4,
        Initialized = 5,
    }

    public enum ScriptThreadState : uint
    {
        NotInScript = 0,
        Running = 1,
    }

    [Flags]
    enum ScriptText : uint
    {
        None = 0x0000,

        DelayExecution = 0x0001,
        IsVisible = 0x0002,
        IsExpression = 0x0020,
        IsPersistent = 0x0040,
        HostManageSource = 0x0080,
    }

    [Flags]
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

    [Flags]
    public enum ScriptInfo : uint
    {
        None = 0x0000,

        IUnknown = 0x0001,
        ITypeInfo = 0x0002,
    }

    [ComVisible (true)]
    [Guid ("BB1A2AE1-A4F9-11cf-8F20-00805F2CD064")]
    [InterfaceType (ComInterfaceType.InterfaceIsIUnknown)]
    public interface IActiveScript
    {
        void SetScriptSite (IActiveScriptSite pass);
        void GetScriptSite (Guid riid, out IntPtr site);
        void SetScriptState (ScriptState state);
        void GetScriptState (out ScriptState scriptState);
        void Close ();
        void AddNamedItem (string name, ScriptItem flags);
        void AddTypeLib (Guid typeLib, uint major, uint minor, uint flags);
        void GetScriptDispatch (string itemName, out IScript dispatch);
        void GetCurrentScriptThreadID (out uint thread);
        void GetScriptThreadID (uint win32ThreadId, out uint thread);
        void GetScriptThreadState (uint thread, out ScriptThreadState state);
        void InterruptScriptThread (uint thread, out System.Runtime.InteropServices.ComTypes.EXCEPINFO exceptionInfo, uint flags);
        void Clone (out IActiveScript script);
    }

    [ComVisible (true), InterfaceType (ComInterfaceType.InterfaceIsIUnknown),
    Guid ("BB1A2AE2-A4F9-11cf-8F20-00805F2CD064")]
    public interface IActiveScriptParse
    {
        void InitNew ();
        void AddScriptlet (string defaultName,
                    string code,
                    string itemName,
                    string subItemName,
                    string eventName,
                    string delimiter,
                    uint sourceContextCookie,
                    uint startingLineNumber,
                    uint flags,
                    out string name,
                    out System.Runtime.InteropServices.ComTypes.EXCEPINFO info);

        void ParseScriptText (
                string code,
                string itemName,
                IntPtr context,
                string delimiter,
                uint sourceContextCookie,
                uint startingLineNumber,
                uint flags,
                IntPtr result,
                out System.Runtime.InteropServices.ComTypes.EXCEPINFO info);
    }
    //
    // A "fake" interface to use to interact with the script itself through IDispatch.
    // It uses the IDispatch guid.  New methods can be added as needed in any order.
    //
    [ComImport]
    [Guid ("00020400-0000-0000-C000-000000000046")]
    [InterfaceType (ComInterfaceType.InterfaceIsIDispatch)]
    public interface IScript
    {
        object FindProxyForURL (string url, string host);
    }

    [InterfaceType (ComInterfaceType.InterfaceIsIUnknown),
 Guid ("DB01A1E3-A42B-11cf-8F20-00805F2CD064")]
    public interface IActiveScriptSite
    {
        void GetLCID (out uint id);
        void GetItemInfo (string pstrName,
                    uint dwReturnMask,
                    [Out, MarshalAs (UnmanagedType.IUnknown)] 
out object item,
                    IntPtr ppti);
        void GetDocVersionString (out string v);
        void OnScriptTerminate (ref object result,
ref System.Runtime.InteropServices.ComTypes.EXCEPINFO info);
        void OnStateChange (uint state);
        void OnScriptError (
[In, MarshalAs (UnmanagedType.IUnknown)] object err);
        void OnEnterScript ();
        void OnLeaveScript ();

    }

    public class ScriptHost : IActiveScriptSite
    {
        public ScriptHost ()
        {
        }

        #region IActiveScriptSite

        public void GetDocVersionString (out string v)
        {
            throw new NotImplementedException ();
        }

        public void GetItemInfo (
[In, MarshalAs (UnmanagedType.BStr)] string pstrName,
            [In, MarshalAs (UnmanagedType.U4)] uint dwReturnMask,
[Out, MarshalAs (UnmanagedType.IUnknown)] out object item,
            IntPtr ppti)
        {
            item = null;
        }

        public void GetLCID (out uint id)
        {
            throw new NotImplementedException ();
        }

        public void OnEnterScript ()
        {
        }

        public void OnLeaveScript ()
        {
        }

        public void OnScriptError ([In, MarshalAs (UnmanagedType.IUnknown)]
 object err)
        {
        }

        public void OnScriptTerminate (ref object result,
ref System.Runtime.InteropServices.ComTypes.EXCEPINFO info)
        {
        }

        public void OnStateChange (uint state)
        {
        }

        #endregion

        public void Run ()
        {
            try {
                object engine = new LScript.LScript ();
                LScript.IActiveScriptParse iap = engine as LScript.IActiveScriptParse;
                iap.InitNew ();

                IActiveScript ias = engine as IActiveScript;
                ias.SetScriptSite (this);

                ias.Close ();
            }
            catch (ExecutionEngineException e) {
                System.Diagnostics.Debug.Assert (false, e.Message);
            }
        }
    }

    class Program
    {


        static void Main (string [] args)
        {
            new ScriptHost ().Run ();

        }
    }
}
