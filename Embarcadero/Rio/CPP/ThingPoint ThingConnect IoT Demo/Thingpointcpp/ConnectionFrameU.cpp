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
#pragma hdrstop

#include "ConnectionFrameU.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TEMSServerConnectionFrame *EMSServerConnectionFrame;
//---------------------------------------------------------------------------
__fastcall TEMSServerConnectionFrame::TEMSServerConnectionFrame(TComponent* Owner)
	: TFrame(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TEMSServerConnectionFrame::ButtonTestConnectionClick(TObject *Sender)
{

  struct TAppHandshakeHandler : public TCppInterfacedObject<TEMSClientAPI::TAppHandshakeProc> {
	void __fastcall Invoke(System::Json::TJSONObject* const AObj)
        {
          ShowMessage(TJson::Format(AObj));
	}
  };
  TEMSClientAPI::_di_TAppHandshakeProc handler = new TAppHandshakeHandler();
  EMSProvider->AppHandshake(handler);
}
//---------------------------------------------------------------------------
void __fastcall TEMSServerConnectionFrame::EditURLHostChange(TObject *Sender)
{
  if (FEMSProvider != NULL)
    FEMSProvider->URLHost = EditURLHost->Text;
}
//---------------------------------------------------------------------------
void __fastcall TEMSServerConnectionFrame::EditURLPortChange(TObject *Sender)
{
  if (FEMSProvider != NULL)
    FEMSProvider->URLPort = StrToInt(EditURLPort->Text);
}

//---------------------------------------------------------------------------
void __fastcall TEMSServerConnectionFrame::Notification(TComponent *AComponent, TOperation Operation)
{
  TComponent::Notification(AComponent, Operation);

  if (Operation == opRemove)
  {
     if (AComponent == FEMSProvider)
       FEMSProvider = NULL;
  }
}

void __fastcall TEMSServerConnectionFrame::SetEMSProvider(
  TEMSProvider * EMSProvider)
{
  if (FEMSProvider != EMSProvider)
  {
    if (FEMSProvider != NULL)
    {
      FEMSProvider->RemoveFreeNotification(this);
    }
    FEMSProvider = EMSProvider;
    if (FEMSProvider != NULL)
    {
      FEMSProvider->FreeNotification(this);
    }
    EMSProviderChanged();
  }
}

void __fastcall TEMSServerConnectionFrame::EMSProviderChanged(void)
{
  if (FEMSProvider != NULL)
  {
    EditURLHost->Text = FEMSProvider->URLHost;
    EditURLPort->Text = IntToStr(FEMSProvider->URLPort);
  }
}

TEMSProvider * __fastcall TEMSServerConnectionFrame::GetEMSProvider(void)
{
  if (FEMSProvider == NULL)
    throw new System::Sysutils::Exception("Missing EMSProvider");
  return FEMSProvider;
}

//---------------------------------------------------------------------------
