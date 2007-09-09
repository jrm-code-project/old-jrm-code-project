using System;
using System.Collections.Generic;
using System.Runtime.InteropServices;
using System.Linq;
using System.Text;

namespace LScript
{
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

    [ComVisible(true)]
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


    [ComVisible(true),InterfaceType (ComInterfaceType.InterfaceIsIUnknown),
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

    [Guid ("DB01A1E3-A42B-11cf-8F20-00805F2CD064")]
    [InterfaceType (ComInterfaceType.InterfaceIsIUnknown)]
    public interface IActiveScriptSite
    {
        void GetLCID (out int lcid);
        void GetItemInfo (
            string name,
            ScriptInfo returnMask,
            [Out] [MarshalAs (UnmanagedType.LPArray, ArraySubType = UnmanagedType.IUnknown)] object [] item,
            [Out] [MarshalAs (UnmanagedType.LPArray)] IntPtr [] typeInfo);
        void GetDocVersionString (out string version);
        void OnScriptTerminate (object result, System.Runtime.InteropServices.ComTypes.EXCEPINFO exceptionInfo);
        void OnStateChange (ScriptState scriptState);
        void OnScriptError (IActiveScriptError scriptError);
        void OnEnterScript ();
        void OnLeaveScript ();
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

    [Guid ("EAE1BA61-A4ED-11cf-8F20-00805F2CD064")]
    [InterfaceType (ComInterfaceType.InterfaceIsIUnknown)]
    public interface IActiveScriptError
    {
        void GetExceptionInfo (out System.Runtime.InteropServices.ComTypes.EXCEPINFO exceptionInfo);
        void GetSourcePosition (out uint sourceContext, out uint lineNumber, out int characterPosition);
        void GetSourceLineText (out string sourceLine);
    }

    [ComVisible(true)]
    [InterfaceTypeAttribute (1)]
    [Guid ("0000010C-0000-0000-C000-000000000046")]
    public interface IPersist
    {
        void GetClassID (out Guid classId);
    }
 
    [ComVisible(true)]
    [Guid("62416980-8B84-4c51-A09C-AB9623C3CC3E")]
    public class LScript : IActiveScript, IActiveScriptParse, IPersist, IScript
    {
        IActiveScriptSite site;
        ScriptState currentScriptState = ScriptState.Uninitialized;

        public LScript ()
        {
            System.Diagnostics.Debug.Assert (false, "LScript()");
        }
         ~LScript ()
        {
            System.Diagnostics.Debug.Assert (false, "Gone!");
        }

        public void SetScriptSite (IActiveScriptSite site)
        {
            System.Diagnostics.Debug.Assert (false, "SetScriptSite()");
            this.site = site;
        }

        public void GetScriptSite (Guid riid, out IntPtr site)
        {
            System.Diagnostics.Debug.Assert (false, "GetScriptSite()");
            site = new IntPtr (0);
        }

        public void SetScriptState (ScriptState state)
        {
            switch (state) {
                case ScriptState.Uninitialized:
                    System.Diagnostics.Debug.Assert (false, "SetScriptState(Uninitialized)");
                    break;
                case ScriptState.Closed:
                    System.Diagnostics.Debug.Assert (false, "SetScriptState(Closed)");
                    break;
                case ScriptState.Connected:
                    System.Diagnostics.Debug.Assert (false, "SetScriptState(Connected)");
                    break;
                case ScriptState.Disconnected:
                    System.Diagnostics.Debug.Assert (false, "SetScriptState(Disconnected)");
                    break;
                case ScriptState.Initialized:
                    System.Diagnostics.Debug.Assert (false, "SetScriptState(Initialised)");
                    break;
                case ScriptState.Started:
                    System.Diagnostics.Debug.Assert (false, "SetScriptState(Started)");
                    break;
                default:
                    System.Diagnostics.Debug.Assert (false, "SetScriptState(??)");
                    break;
            }

            this.currentScriptState = state;
        }

        public void GetScriptState (out ScriptState scriptState)
        {
            System.Diagnostics.Debug.Assert (false, "GetScriptState()");
            scriptState = this.currentScriptState;
        }

        public void Close ()
        {
            System.Diagnostics.Debug.Assert (false, "Close()");
        }
        public void AddNamedItem (string name, ScriptItem flags)
        {
            System.Diagnostics.Debug.Assert (false, "AddNamedItem(\"" + name + "\")");
        }
        public void AddTypeLib (Guid typeLib, uint major, uint minor, uint flags)
        {
            System.Diagnostics.Debug.Assert (false, "AddTypeLib()");
        }

        public void GetScriptDispatch (string itemName, out IScript dispatch)
        {
            System.Diagnostics.Debug.Assert (false, "GetScriptDispatch()");
            dispatch = this;
        }
        public void GetCurrentScriptThreadID (out uint thread)
        {
            System.Diagnostics.Debug.Assert (false, "GetCurrentScriptThreadID()");
            thread = 0;
        }
        public void GetScriptThreadID (uint win32ThreadId, out uint thread)
        {
            System.Diagnostics.Debug.Assert (false, "GetScriptThreadID()");
            thread = 0;
        }
        public void GetScriptThreadState (uint thread, out ScriptThreadState state)
        {
            System.Diagnostics.Debug.Assert (false, "GetScriptThreadState()");
            state = ScriptThreadState.NotInScript;
        }
        public void InterruptScriptThread (uint thread, out System.Runtime.InteropServices.ComTypes.EXCEPINFO exceptionInfo, uint flags)
        {
            System.Diagnostics.Debug.Assert (false, "InterruptScriptThread()");
            exceptionInfo = new System.Runtime.InteropServices.ComTypes.EXCEPINFO ();
        }

        public void Clone (out IActiveScript script)
        {
            System.Diagnostics.Debug.Assert (false, "Clone()");
            script = this;
        }

        public void InitNew ()
        {
            System.Diagnostics.Debug.Assert (false, "InitNew()");
                this.currentScriptState = ScriptState.Disconnected;
                try {
                    if (this.site != null)
                        this.site.OnStateChange (this.currentScriptState);
                }
                catch (Exception e) {
                    System.Diagnostics.Debug.Assert (false, e.Message);
                }
                finally {
                System.Diagnostics.Debug.Assert (false, "Leaving.");
            }
         
        }

        public void AddScriptlet (string defaultName,
                    string code,
                    string itemName,
                    string subItemName,
                    string eventName,
                    string delimiter,
                    uint sourceContextCookie,
                    uint startingLineNumber,
                    uint flags,
                    out string name,
                    out System.Runtime.InteropServices.ComTypes.EXCEPINFO info)
        {
            System.Diagnostics.Debug.Assert (false, "AddScriptlet");
            name = "";
            info = new System.Runtime.InteropServices.ComTypes.EXCEPINFO ();
        }

        public void ParseScriptText (
                string code,
                string itemName,
                IntPtr context,
                string delimiter,
                uint sourceContextCookie,
                uint startingLineNumber,
                uint flags,
                IntPtr result,
                out System.Runtime.InteropServices.ComTypes.EXCEPINFO info)
        {
            System.Diagnostics.Debug.Assert (false, "ParseScriptText");
            info = new System.Runtime.InteropServices.ComTypes.EXCEPINFO ();

        }

        public void GetClassID (out Guid classId)
        {
            System.Diagnostics.Debug.Assert (false, "GetClassID()");
            classId = new Guid ("62416980-8B84-4c51-A09C-AB9623C3CC3E");
        }

        public object FindProxyForURL (string a, string b)
        {
            System.Diagnostics.Debug.Assert (false, "FindProxyForURL()");
            return this;
        }

    }
}
