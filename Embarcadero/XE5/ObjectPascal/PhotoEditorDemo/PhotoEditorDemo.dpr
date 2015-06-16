//---------------------------------------------------------------------------

// This software is Copyright (c) 2013 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
program PhotoEditorDemo;

uses
  System.StartUpCopy,
  FMX.Forms,
  {$IFDEF IOS}
  iOSapi.UIKit,
  {$ENDIF }
  {$IFDEF ANDROID}
  FMX.Platform.Android, Androidapi.JNI.GraphicsContentViewText,
  {$ENDIF }
  MainFrm in 'MainFrm.pas' {BaseMainForm},
  MainFrm_Tablet in 'MainFrm_Tablet.pas' {PadMainForm},
  MainFrm_Phone in 'MainFrm_Phone.pas' {PhoneMainForm};

{$R *.res}
{$IFDEF IOS}
function IsTablet: Boolean;
begin
  Result := TUIDevice.Wrap(TUIDevice.OCClass.currentDevice).userInterfaceIdiom = UIUserInterfaceIdiomPad;
end;
{$ENDIF}
{$IFDEF ANDROID}
function IsTablet: Boolean;
begin
  Result := (MainActivity.getResources.getConfiguration.screenLayout and TJConfiguration.JavaClass.SCREENLAYOUT_SIZE_MASK)
    >= TJConfiguration.JavaClass.SCREENLAYOUT_SIZE_LARGE;
end;
{$ENDIF}

begin
  Application.Initialize;
  if IsTablet then
    Application.CreateForm(TTabletMainForm, TabletMainForm)
  else
    Application.CreateForm(TPhoneMainForm, PhoneMainForm);

  Application.Run;
end.

