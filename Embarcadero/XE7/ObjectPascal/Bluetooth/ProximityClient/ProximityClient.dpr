program ProximityClient;

uses
  System.StartUpCopy,
  FMX.Forms,
  UProximityClient in 'UProximityClient.pas' {frmProximityForm},
   {$IFDEF MSWINDOWS}
  System.Win.Bluetooth in 'C:\dev\tp\runtime\rtl\common\System.Win.Bluetooth.pas',
  Winapi.BluetoothLE in 'Winapi.BluetoothLE.pas',
  {$ENDIF }
  {$IFDEF ANDROID}
  System.Android.Bluetooth in 'C:\dev\tp\runtime\rtl\common\System.Android.Bluetooth.pas',
  {$ENDIF }
  {$IFDEF MACOS}
  System.Mac.Bluetooth in 'C:\dev\tp\runtime\rtl\common\System.Mac.Bluetooth.pas',
  {$ENDIF}
  System.Bluetooth in 'C:\dev\tp\runtime\rtl\common\System.Bluetooth.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmProximityForm, frmProximityForm);
  Application.Run;
end.
