//---------------------------------------------------------------------------

// This software is Copyright (c) 2015 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of an Embarcadero developer tools product.
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

#include <fmx.h>
#include <System.Beacon.hpp>
#include <System.SysUtils.hpp>
#include <System.Math.hpp>
#include <FMX.DialogService.hpp>
#pragma hdrstop

#include "Unit1.h"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
    : TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::BtnAddRegionClick(TObject *Sender)
{
  String LItemString;
  String LStringGuid;
  Integer LMajor;
  Integer LMinor;
  TDummyInt *LNumberObj;
  CheckManager();
  for (int I = 0; I < ListBox1->Items->Count; I++) {
	LItemString = (*ListBox1->Items)[I];
	StringToRegion(LItemString, LStringGuid, LMajor, LMinor);
	if (LStringGuid == EdGuid->Text) {
	  if ((Floor(sbMajor->Value) == LMajor) && (Floor(sbMinor->Value) == LMinor))
		return;
	  else
	  {
		LNumberObj = new TDummyInt();
		LNumberObj->Number = I;
		TDialogService::MessageDialog("This region will replace previous region with same GUID " + LStringGuid,
			TMsgDlgType::mtConfirmation, mbOKCancel, TMsgDlgBtn::mbCancel, 0, BtnAddRegionCloseEvent, LNumberObj);
        return;
      }
    }
  }
  AddRegion();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::BtnAddRegionCloseEvent(TObject *Sender,  const TModalResult AResult)
{
  int I = static_cast<TDummyInt*>(Sender)->Number;
  Sender->Free();
  if(AResult != mrOk)
	return;
  ListBox1->Items->Delete(I);
  AddRegion();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::AddRegion()
{
  String LNewItem;
  BOOL ShouldAdd;
  String LStringGuid;

  CheckManager();

  LNewItem = EdGuid->Text + ";" + IntToStr((int) Floor(sbMajor->Value)) + ";" + IntToStr((int) Floor(sbMinor->Value));
  if (ListBox1->Items->IndexOf(LNewItem) < 0) {
    ShouldAdd = False;
    if (sbMajor->Value > -1) {
      if (sbMinor->Value > -1)
        ShouldAdd = FBeaconManager->RegisterBeacon(StringToGUID(EdGuid->Text), Floor(sbMajor->Value), Floor(sbMinor->Value));
      else
        ShouldAdd = FBeaconManager->RegisterBeacon(StringToGUID(EdGuid->Text), Floor(sbMajor->Value));
    }
    else
      ShouldAdd = FBeaconManager->RegisterBeacon(StringToGUID(EdGuid->Text));
    if (ShouldAdd)
      ListBox1->Items->Add(LNewItem);
  }
}

void __fastcall TForm1::CheckManager()
{
  if (FBeaconManager == NULL)
  {
    FBeaconManager = TBeaconManager::GetBeaconManager((TBeaconScanMode)ComboBox1->ItemIndex);
    FBeaconManager->OnBeaconEnter = BeaconEnter;
    FBeaconManager->OnBeaconExit = BeaconExit;
    FBeaconManager->OnEnterRegion = EnterRegion;
    FBeaconManager->OnExitRegion = ExitRegion;
    FBeaconManager->OnBeaconProximity = BeaconProximity;
  }
}

void __fastcall TForm1::BeaconEnter(System::TObject* const Sender, const _di_IBeacon ABeacon, const TBeaconList CurrentBeaconList)
{
  TListViewItem *LItem;
  LItem = LvEnteredBeacon->Items->Add();
  LItem->Text = GUIDToString(ABeacon->GUID);
  LItem->Detail = "Major: " + IntToStr(ABeacon->Major) + " Minor: " + IntToStr(ABeacon->Minor) + " time :" + TimeToStr(Now()) ;

  TMonitor::Enter(FLock);
  try
  {
    FCurrentBeaconList = CurrentBeaconList;
  }
  __finally
  {
    TMonitor::Exit(FLock);
  }
}

void __fastcall TForm1::BeaconExit(System::TObject* const Sender, const _di_IBeacon ABeacon, const TBeaconList CurrentBeaconList)
{
  TListViewItem *LItem;
  LItem = LvExitedBeacon->Items->Add();
  LItem->Text = GUIDToString(ABeacon->GUID);
  LItem->Detail = "Major: " + IntToStr(ABeacon->Major) + " Minor: " + IntToStr(ABeacon->Minor) + " time :" + TimeToStr(Now()) ;

  TMonitor::Enter(FLock);
  try
  {
    FCurrentBeaconList = CurrentBeaconList;
  }
  __finally
  {
    TMonitor::Exit(FLock);
  }
}

void __fastcall TForm1::EnterRegion(System::TObject* const Sender, const GUID &UUID, int AMajor, int AMinor)
{
  TListViewItem *LItem;
  LItem = LvEnteredRegion->Items->Add();
  LItem->Text = GUIDToString(UUID);
  LItem->Detail = "Major: " + IntToStr(AMajor) + " Minor: " + IntToStr(AMinor) + " time :" + TimeToStr(Now());
}

void __fastcall TForm1::ExitRegion(System::TObject* const Sender, const GUID &UUID, int AMajor, int AMinor)
{
  TListViewItem *LItem;
  LItem = LvExitedRegion->Items->Add();
  LItem->Text = GUIDToString(UUID);
  LItem->Detail = "Major: " + IntToStr(AMajor) + " Minor: " + IntToStr(AMinor) + " time :" + TimeToStr(Now());
}

void __fastcall TForm1::BeaconProximity(System::TObject* const Sender, const _di_IBeacon ABeacon, TBeaconProximity Proximity)
{
  String LProximityText;
  TListViewItem *LNewitem;
  Integer I;

  switch (Proximity)
  {
    case TBeaconProximity::Immediate: LProximityText = "Inmediate";
    case TBeaconProximity::Near: LProximityText = "Near2";
    case TBeaconProximity::Far: LProximityText = "Far";
    case TBeaconProximity::Away: LProximityText = "Away";
  }

  LNewitem = LvProximity->Items->Add();
  LNewitem->Text = GUIDToString(ABeacon->GUID);
  LNewitem->Detail = " Ma:" + IntToStr(ABeacon->Major) + " Mi:" + IntToStr(ABeacon->Minor) + " Dist:" + FloatToStr(ABeacon->Distance)
                      + "Proximity: " +  LProximityText + " time " + TimeToStr(Now());

  for( I = 0; I < FList.Length; I++)
  {
        if ( FList[I].FName == (GUIDToString(ABeacon->GUID) + ";" + IntToStr(ABeacon->Major)+ ";" + IntToStr(ABeacon->Minor)))
        {
          switch (Proximity)
          {
            case TBeaconProximity::Immediate: FList[I].FOriginalColor = TAlphaColorRec::Green; break;
            case TBeaconProximity::Near: FList[I].FOriginalColor = TAlphaColorRec::Yellow; break;
            case TBeaconProximity::Far: FList[I].FOriginalColor = TAlphaColorRec::Red; break;
            case TBeaconProximity::Away: FList[I].FOriginalColor = TAlphaColorRec::Black; break;
          }
          break;
        }
  }
}

//---------------------------------------------------------------------------
void __fastcall TForm1::BtnDeleteRegionClick(TObject *Sender)
{
  String LStringGuid;
  int LMajor;
  int LMinor;
  bool LUnregistered;
  CheckManager();
  if (ListBox1->ItemIndex >= 0)
  {
        StringToRegion(ListBox1->Items->Strings[ListBox1->ItemIndex], LStringGuid, LMajor, LMinor);
        if (LMajor == -1)
          LUnregistered = FBeaconManager->UnregisterBeacon(StringToGUID(LStringGuid));
        else if (LMinor == -1)
          LUnregistered = FBeaconManager->UnregisterBeacon(StringToGUID(LStringGuid), LMajor);
        else
          LUnregistered = FBeaconManager->UnregisterBeacon(StringToGUID(LStringGuid), LMajor, LMinor);

        if (LUnregistered)
          ListBox1->Items->Delete(ListBox1->ItemIndex);
  }
}

void __fastcall TForm1::StringToRegion(String AString, String &Guid,int &Major, int &Minor)
{
  TStringDynArray LSplitted = System::Strutils::SplitString(AString,';');
  Guid = LSplitted[0];
  Major = StrToInt(LSplitted[1]);
  Minor = StrToInt(LSplitted[2]);
}

//---------------------------------------------------------------------------
void __fastcall TForm1::Button1Click(TObject *Sender)
{
  CheckManager();
  if (FBeaconManager->StartScan())
    Timer1->Enabled = True;

  else
    ShowMessage("Cannot start to scan beacons");
}
//---------------------------------------------------------------------------

void __fastcall TForm1::btnStopClick(TObject *Sender)
{
  CheckManager();
  if (FBeaconManager->StopScan() == False)
    ShowMessage("Cannot stop to scan beacons");
  Timer1->Enabled = False;
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button2Click(TObject *Sender)
{
  LvEnteredBeacon->Items->Clear();
  LvExitedBeacon->Items->Clear();
  LvEnteredRegion->Items->Clear();
  LvExitedRegion->Items->Clear();
  LvMonitoring->Items->Clear();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ComboBox1Change(TObject *Sender)
{
  int I;
  String LGuid;
  int LMajor;
  int LMinor;

  Timer1->Enabled = False;
  if (FBeaconManager != NULL)
  {
    FBeaconManager->StopScan();
    FBeaconManager->Free();
    FBeaconManager = NULL;
    FCurrentBeaconList.Length = 0;
  }
    CheckManager();
    for(I = 0; I < ListBox1->Count; I++)
    {
      StringToRegion(ListBox1->Items->Strings[I], LGuid, LMajor, LMinor);
      if (LMajor == -1)
        FBeaconManager->RegisterBeacon(StringToGUID(LGuid));
      else if (LMinor == -1)
        FBeaconManager->RegisterBeacon(StringToGUID(LGuid), LMajor);
      else
        FBeaconManager->RegisterBeacon(StringToGUID(LGuid), LMajor, LMinor);
    }
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Timer1Timer(TObject *Sender)
{
  int I;
  TListViewItem *LItem;
  TMonitor::Enter(FLock);
  try
  {
    LvMonitoring->Items->Clear();
    FList.Length = FCurrentBeaconList.Length;

    for (I = 0; I < FList.Length; I++)
    {
	  if ((FCurrentBeaconList[I] != NULL) && (FCurrentBeaconList[I]->ItsAlive()))
      {
        LItem = LvMonitoring->Items->Add();
        LItem->Text = GUIDToString(FCurrentBeaconList[I]->GUID);
        LItem->Detail = "Major: " + IntToStr(FCurrentBeaconList[I]->Major)+ " Minor: " + IntToStr(FCurrentBeaconList[I]->Minor) +
                    "Proximity: " + IntToStr(FCurrentBeaconList[I]->Proximity) + Char(13) +
                   "Rssi: " + IntToStr(FCurrentBeaconList[I]->Rssi) + " Distance: " + FloatToStr(FCurrentBeaconList[I]->Distance);
        FList[I].FOriginalColor = TAlphaColorRec::Blue;
        FList[I].FDistance = FCurrentBeaconList[I]->Distance;
        FList[I].FName = GUIDToString(FCurrentBeaconList[I]->GUID) + ";" + IntToStr(FCurrentBeaconList[I]->Major) + ";" + IntToStr(FCurrentBeaconList[I]->Minor);
        switch (FCurrentBeaconList[I]->Proximity)
        {
          case TBeaconProximity::Immediate: FList[I].FOriginalColor = TAlphaColorRec::Green; break;
          case TBeaconProximity::Near: FList[I].FOriginalColor = TAlphaColorRec::Yellow; break;
          case TBeaconProximity::Far: FList[I].FOriginalColor = TAlphaColorRec::Red; break;
          case TBeaconProximity::Away: FList[I].FOriginalColor = TAlphaColorRec::Black; break;
        }
      }
    }
    BeaconsRectangle->Repaint();
  }
  __finally
  {
    TMonitor::Exit(FLock);
  }

}
//---------------------------------------------------------------------------
void __fastcall TForm1::BeaconsRectangleMouseUp(TObject *Sender, TMouseButton Button, TShiftState Shift, float X, float Y)
{
  String LName;
  TPointF LPoint;
  LPoint.X = X;
  LPoint.Y = Y;
  LName = FRenderer->GetObjectUnderMouse(BeaconsRectangle, LPoint);

  if (LName != "")
  {
    if (FSelectedBeacon == LName)
      FSelectedBeacon = "";
    else
      FSelectedBeacon = LName;
    BeaconsRectangle->Repaint();
  }
}
//---------------------------------------------------------------------------

void __fastcall TForm1::BeaconsRectanglePaint(TObject *Sender, TCanvas *Canvas, const TRectF &ARect)
{
  int I;
  String LGuid;
  int LMajor;
  int LMinor;

  if (FSelectedBeacon != "")
  {
    StringToRegion(FSelectedBeacon, LGuid, LMajor, LMinor);
    LbUUID->Text = LGuid;
    LbMajor->Text = IntToStr(LMajor);
    LbMinor->Text = IntToStr(LMinor);
    try
    {
      TMonitor::Enter(FLock);
      for (I = 0; I < FCurrentBeaconList.Length; I++)
	  {
		if ((FCurrentBeaconList[I] != NULL) && (FCurrentBeaconList[I]->ItsAlive()))
		  if (( GUIDToString(FCurrentBeaconList[I]->GUID) == LGuid) && (LMajor == FCurrentBeaconList[I]->Major)
			  && (LMinor == FCurrentBeaconList[I]->Minor) )
		  {
			LbDistance->Text = FloatToStr(FCurrentBeaconList[I]->Distance);
			LbRssi->Text = IntToStr(FCurrentBeaconList[I]->Rssi);
			break;
          }
      }
    }
    __finally
    {
      TMonitor::Exit(FLock);
    }
  }

  for (I = 0; I < FList.Length; I++)
  {
    if (FList[I].FName == FSelectedBeacon)
      FList[I].FColor = TAlphaColorRec::Blueviolet;
    else
      FList[I].FColor = FList[I].FOriginalColor;
  }
  FRenderer->Render(FMaxDistance, FList, BeaconsRectangle, Canvas, ARect);

}
//---------------------------------------------------------------------------

void __fastcall TForm1::FormCreate(TObject *Sender)
{
  FLock = new TObject;
  FRenderer = new TRenderer;
  FMaxDistance = 30;

  CheckManager();

  if (FBeaconManager->RegisterBeacon(StringToGUID("{B9407F30-F5F8-466E-AFF9-25556B57FE6D}")))
      ListBox1->Items->Add("{B9407F30-F5F8-466E-AFF9-25556B57FE6D};-1;-1");
  if (FBeaconManager->RegisterBeacon(StringToGUID("{BFC8442B-819F-40C9-B56A-8B37FB9421E0}")))
      ListBox1->Items->Add("{BFC8442B-819F-40C9-B56A-8B37FB9421E0};-1;-1");
  if (FBeaconManager->RegisterBeacon(StringToGUID("{2F234454-CF6D-4A0F-ADF2-F4911BA9FFA6}")))
      ListBox1->Items->Add("{2F234454-CF6D-4A0F-ADF2-F4911BA9FFA6};-1;-1");
  if (FBeaconManager->RegisterBeacon(StringToGUID("{699EBC80-E1F3-11E3-9A0F-0CF3EE3BC012}")))
      ListBox1->Items->Add("{699EBC80-E1F3-11E3-9A0F-0CF3EE3BC012};-1;-1");
}
//---------------------------------------------------------------------------

void __fastcall TForm1::SpinBox1Change(TObject *Sender)
{
  FMaxDistance = SpinBox1->Value;
}
//---------------------------------------------------------------------------

void __fastcall TForm1::sbMajorChange(TObject *Sender)
{
  if (sbMajor->Value == -1) {
    sbMinor->Value = -1;
  }
  if ((sbMajor->Value >= 0) && (sbMinor->Value == -1)) {
    sbMinor->Value = 0;
  }
}
//---------------------------------------------------------------------------

void __fastcall TForm1::ListBox1Click(TObject *Sender)
{
  String LGuid;
  String LItemString;
  int LMajor;
  int LMinor;
  if (ListBox1->ItemIndex >= 0)
  {
	LItemString = (*ListBox1->Items)[ListBox1->ItemIndex];
	StringToRegion(LItemString, LGuid, LMajor, LMinor);
	EdGuid->Text = LGuid;
	sbMajor->Value = LMajor;
	sbMinor->Value = LMinor;
  };
}
//---------------------------------------------------------------------------

