//---------------------------------------------------------------------------
// Copyright (c) 2016 Embarcadero Technologies, Inc. All rights reserved.  
//
// This software is the copyrighted property of Embarcadero Technologies, Inc. 
// ("Embarcadero") and its licensors. You may only use this software if you 
// are an authorized licensee of Delphi, C++Builder or RAD Studio 
// (the "Embarcadero Products").  This software is subject to Embarcadero's 
// standard software license and support agreement that accompanied your 
// purchase of the Embarcadero Products and is considered a Redistributable, 
// as such term is defined thereunder. Your use of this software constitutes 
// your acknowledgement of your agreement to the foregoing software license 
// and support agreement. 
//---------------------------------------------------------------------------

#include <fmx.h>
#include <EMSHosting.EdgeHttpListener.hpp>
#ifdef WIN32
#include <Winapi.Winsock2.hpp>
#endif
#ifdef __linux__
#include <Posix.Unistd.hpp>
#endif
#pragma hdrstop

#include "ListenerFrameU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TEMSEdgeModuleListenerFrame *EMSEdgeModuleListenerFrame;
//---------------------------------------------------------------------------
__fastcall TEMSEdgeModuleListenerFrame::TEMSEdgeModuleListenerFrame(TComponent* Owner)
	: TFrame(Owner)
{
}
//---------------------------------------------------------------------------

String GetLocalHost(void)
{
	String result;
    char bytes[256];
    if (gethostname(bytes, 255) == 0) {
      result = bytes;
    }
    return result;
}

void __fastcall TEMSEdgeModuleListenerFrame::SetEMSEdgeService(
  TEMSEdgeService * Value)
{
  if (FEMSEdgeService != Value)
  {
    if (FEMSEdgeService != NULL)
    {
      FEMSEdgeService->RemoveFreeNotification(this);
    }
    FEMSEdgeService = Value;
    if (FEMSEdgeService != NULL)
    {
      FEMSEdgeService->FreeNotification(this);
    }
    EdgeServiceChanged();
  }
}

void __fastcall TEMSEdgeModuleListenerFrame::SetEMSProvider(
  TEMSProvider * Value)
{
  if (FEMSProvider != Value)
  {
    if (FEMSProvider != NULL)
    {
      FEMSProvider->RemoveFreeNotification(this);
    }
    FEMSProvider = Value;
    if (FEMSProvider != NULL)
    {
      FEMSProvider->FreeNotification(this);
    }
  }
}

void __fastcall TEMSEdgeModuleListenerFrame::ActionActivateEdgeModuleExecute(
  TObject * Sender)
{
  assert(FEMSEdgeService != NULL);
  UpdateEdgeService();
  assert(EMSEdgeService->AutoRegister);
  EMSEdgeService->Active = ! EMSEdgeService->Active;  // Register or unregister
}

void __fastcall TEMSEdgeModuleListenerFrame::ActionActivateEdgeModuleUpdate(
  TObject * Sender)
{
  ((TAction*)Sender)->Checked = (FEMSEdgeService != NULL) &&  EMSEdgeService->Active;
  ((TAction*)Sender)->Enabled = (FEMSEdgeService != NULL) && (EditModuleName->Text != "");
  EditModuleName->Enabled = ! ((FEMSEdgeService != NULL) &&  EMSEdgeService->Active);
}

void __fastcall TEMSEdgeModuleListenerFrame::ButtonGetLocalClick(TObject * Sender)
{
  EditListenerHost->Text = GetLocalHost();
}

void __fastcall TEMSEdgeModuleListenerFrame::ButtonTestVersionClick(TObject * Sender)
{
  _di_IBackendProvider ProviderIntf;
  Supports(EMSProvider, __uuidof(IBackendProvider), &ProviderIntf);
  assert(ProviderIntf != NULL);
  BackendEndpointVersion->Provider = ProviderIntf; // EMSProvider->QueryInterface();
  BackendEndpointVersion->Params->Items[0]->Value = EditModuleName->Text;
  BackendEndpointVersion->Execute();
  ShowMessage(TJson::Format(BackendEndpointVersion->Response->JSONValue));
}


void __fastcall TEMSEdgeModuleListenerFrame::Notification(TComponent *AComponent, TOperation Operation)
{
  TComponent::Notification(AComponent, Operation);

  if (Operation == opRemove)
  {
     if (AComponent == FEMSEdgeService)
       FEMSEdgeService = NULL;
     if (AComponent == FEMSProvider)
       FEMSProvider = NULL;
  }
}

void __fastcall TEMSEdgeModuleListenerFrame::UpdateEdgeService(void)
{
  EMSEdgeService->ModuleName = EditModuleName->Text;
  TCustomEMSEdgeHTTPListener* listener = dynamic_cast<TCustomEMSEdgeHTTPListener*>(EMSEdgeService->ListenerService);
  assert(listener != NULL);
  listener->Host = EditListenerHost->Text;
  listener->Port = StrToInt(EditListenerPort->Text);
}


void __fastcall TEMSEdgeModuleListenerFrame::EdgeServiceChanged()
{
  if (FEMSEdgeService != NULL)
  {
    TCustomEMSEdgeHTTPListener* listener = dynamic_cast<TCustomEMSEdgeHTTPListener*>(EMSEdgeService->ListenerService);
    assert(listener != NULL);

    EditListenerPort->Text = IntToStr((int)(listener->Port));

    EditListenerHost->Text = listener->Host;

    // Fill in module parameter for endpoint
    for(int i = 0; i++; i < ComponentCount - 1)
    {
      TBackendEndpoint * endpoint =  dynamic_cast<TBackendEndpoint*>(Components[i]);
      if (endpoint != NULL)
      {
 	if (endpoint->Params->ContainsParameter("module"))
		endpoint->Params->ParameterByName("module")->Value = EMSEdgeService->ModuleName;
      }
    }

    if (EditListenerHost->Text == "")
      // Get host name for this computer
      EditListenerHost->Text = GetLocalHost();
  }
}

