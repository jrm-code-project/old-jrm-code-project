// ScriptHost.cpp : Defines the entry point for the console application.
//

#include "stdafx.h"
#include "MyScriptSite.h"

int _tmain(int argc, _TCHAR* argv[])
{
	// Declaration
    IActiveScript* activscp;
 
    IActiveScriptSite* activscpsite = NULL;


	CoInitialize(NULL);
	CLSID lscript_clsid;
	HRESULT hr = CLSIDFromProgID(TEXT("LScript"),&lscript_clsid);

    hr = CoCreateInstance(lscript_clsid, NULL,
        CLSCTX_INPROC_SERVER, IID_IActiveScript,
        (void **)&activscp);

	hr = activscp->SetScriptSite(activscpsite);
	hr = activscp->SetScriptState(SCRIPTSTATE_INITIALIZED);
	hr = activscp->AddNamedItem(TEXT("ANamedItem"),42);

	return 0;
}

