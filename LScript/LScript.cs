﻿using System;
using System.Runtime.InteropServices;

namespace LScript
{
    //
    // Scripting Interfaces
    //
    [ComVisible(true)]
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

    [ComVisible(true)]
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

    [ComVisible(true),
    InterfaceType (ComInterfaceType.InterfaceIsIUnknown),
     Guid ("BB1A2AE1-A4F9-11cf-8F20-00805F2CD064")]
    public interface IActiveScript
    {
        void SetScriptSite ([In, MarshalAs (UnmanagedType.Interface)]
 IActiveScriptSite site);
        void GetScriptSite (ref System.Guid riid,
out System.IntPtr ppvObject);
        void SetScriptState ([In, MarshalAs (UnmanagedType.U4)] ScriptState ss);
        void GetScriptState ([Out, MarshalAs (UnmanagedType.U4)] out ScriptState ss);
        void Close ();
        void AddNamedItem ([In, MarshalAs (UnmanagedType.BStr)] 
string pstrName, [In, MarshalAs (UnmanagedType.U4)] uint dwFlags);
        void AddTypeLib (ref System.Guid rguidTypeLib, uint dwMajor,
uint dwMinor, uint dwFlags);
        void GetScriptDispatch (string pstrItemName, out object ppdisp);
        void GetCurrentScriptThreadID (out uint id);
        void GetScriptThreadID (uint threadid, out uint id);
        void GetScriptThreadState (uint id, [Out, MarshalAs(UnmanagedType.U4)] out ScriptThreadState state);
        void InterruptScriptThread (uint id,
ref stdole.EXCEPINFO info,
uint flags);
        void Clone (out IActiveScript item);
    };


    [ComVisible (true)]
    [InterfaceType (ComInterfaceType.InterfaceIsIUnknown),
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
                    out stdole.EXCEPINFO info);

        void ParseScriptText (
                string code,
                string itemName,
                IntPtr context,
                string delimiter,
                uint sourceContextCookie,
                uint startingLineNumber,
                uint flags,
                IntPtr result,
                out stdole.EXCEPINFO info);
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
                    ref stdole.EXCEPINFO info);
        void OnStateChange (uint state);
        void OnScriptError (
            [In, MarshalAs (UnmanagedType.IUnknown)] object err);
        void OnEnterScript ();
        void OnLeaveScript ();
    }


    [ComImport]
    [ComVisible(true)]
    [Guid("1CFF0050-6FDD-11d0-9328-00A0C90DCAA9")]
     [InterfaceType (ComInterfaceType.InterfaceIsIUnknown)]
    public interface IActiveScriptParseProcedure {

           void ParseProcedureText (string code,
                string formalParameters,
               string itemName,
               IntPtr context,
               string delimiter,
               uint sourceContextCookie,
               uint startingLineNumber,
               uint flags,
               out IScript dispatch);
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

    [ComVisible(true)]
    [InterfaceTypeAttribute (1)]
    [Guid ("0000010C-0000-0000-C000-000000000046")]
    public interface IPersist
    {
        void GetClassID (out Guid classId);
    }
 
    [ComVisible(true)]
    [Guid("62416980-8B84-4c51-A09C-AB9623C3CC3E")]
    [ClassInterface (ClassInterfaceType.AutoDispatch)]
    [ProgId ("LScript")]
    public class LScript : IActiveScript, IActiveScriptParse, IScript
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

        public void GetScriptSite (ref Guid riid, out IntPtr site)
        {
            System.Diagnostics.Debug.Assert (false, "GetScriptSite()");
            site = new IntPtr (0);
        }

        public void SetScriptState (ScriptState state)
        {
            System.Diagnostics.Debug.Assert (false, "SetScriptState()");

            this.currentScriptState =  state;
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
        public void AddNamedItem (string name, uint flags)
        {
            System.Diagnostics.Debug.Assert (false, "AddNamedItem()");
        }
        public void AddTypeLib (ref Guid typeLib, uint major, uint minor, uint flags)
        {
            System.Diagnostics.Debug.Assert (false, "AddTypeLib()");
        }

        public void GetScriptDispatch (string itemName, out object dispatch)
        {
            System.Diagnostics.Debug.Assert (false, "GetScriptDispatch()");
            dispatch = this as IScript;
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
        public void InterruptScriptThread (uint thread, ref stdole.EXCEPINFO exceptionInfo, uint flags)
        {
            System.Diagnostics.Debug.Assert (false, "InterruptScriptThread()");
            exceptionInfo = new stdole.EXCEPINFO ();
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
                        this.site.OnStateChange ((uint)this.currentScriptState);
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
                    out stdole.EXCEPINFO info)
        {
            System.Diagnostics.Debug.Assert (false, "AddScriptlet");
            name = "";
            info = new stdole.EXCEPINFO ();
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
                out stdole.EXCEPINFO info)
        {
            System.Diagnostics.Debug.Assert (false, "ParseScriptText");
            info = new stdole.EXCEPINFO ();

        }

        public void ParseProcedureText (
        string code,
        string itemName,
            string objectName,
        IntPtr context,
        string delimiter,
        uint sourceContextCookie,
        uint startingLineNumber,
        uint flags,
        out IScript result)
        {
            System.Diagnostics.Debug.Assert (false, "ParseProcedureText");
            result = this;

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
