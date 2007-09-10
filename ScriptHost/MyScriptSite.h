 #include <windows.h>
   #include <ActivScp.h>

   class CMyScriptSite : public IActiveScriptSite {
   private:
      ULONG m_dwRef;
   public:
      IUnknown *m_pUnkScriptObject;
      ITypeInfo *m_pTypeInfo;

      CMyScriptSite::CMyScriptSite() {
         m_pUnkScriptObject = 0;
         m_pTypeInfo = 0;
         m_dwRef = 1;
      }

      // IUnknown methods...
      virtual HRESULT __stdcall QueryInterface(REFIID riid,
         void **ppvObject) {
         *ppvObject = NULL;
         return E_NOTIMPL;
      }
      virtual ULONG _stdcall AddRef(void) {
         return ++m_dwRef;
      }
      virtual ULONG _stdcall Release(void) {
         if(--m_dwRef == 0) return 0;
         return m_dwRef;
      }

      // IActiveScriptSite methods...
      virtual HRESULT __stdcall GetLCID(LCID *plcid) {
         return S_OK;
      }
   
      virtual HRESULT __stdcall GetItemInfo(LPCOLESTR pstrName,
         DWORD dwReturnMask, IUnknown **ppunkItem, ITypeInfo **ppti) {
         // Is it expecting an ITypeInfo?
         if(ppti) {
            // Default to NULL.
            *ppti = NULL;
         
            // See if asking about ITypeInfo... 
            if(dwReturnMask & SCRIPTINFO_ITYPEINFO) {
               *ppti = m_pTypeInfo;
            }
         }
      
         // Is the engine passing an IUnknown buffer?
         if(ppunkItem) {
            // Default to NULL.
            *ppunkItem = NULL;
         
            // Is Script Engine looking for an IUnknown for our object?
            if(dwReturnMask & SCRIPTINFO_IUNKNOWN) {
               // Check for our object name...
               if (!_wcsicmp(L"MyObject", pstrName)) {
                  // Provide our object.
                  *ppunkItem = m_pUnkScriptObject;
                  // Addref our object...
                  m_pUnkScriptObject->AddRef();
               }
            }
         }
      
         return S_OK;
      }
   
      virtual HRESULT __stdcall GetDocVersionString(BSTR *pbstrVersion) {
         return S_OK;
      }
   
      virtual HRESULT __stdcall OnScriptTerminate(
         const VARIANT *pvarResult, const EXCEPINFO *pexcepInfo) {
         return S_OK;
      }
   
      virtual HRESULT __stdcall OnStateChange(SCRIPTSTATE ssScriptState) {
         return S_OK;
      }
   
      virtual HRESULT __stdcall OnScriptError(
         IActiveScriptError *pscriptError) {
         ::MessageBox(NULL, TEXT("OnScriptError()"), TEXT("Error"), MB_SETFOREGROUND);     
         return S_OK;
      }
   
      virtual HRESULT __stdcall OnEnterScript(void) {
         return S_OK;
      }
   
      virtual HRESULT __stdcall OnLeaveScript(void) {
         return S_OK;
      }

   };
