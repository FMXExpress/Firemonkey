unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Memo;

type
  TFormMain = class(TForm)
    ButtonInfo: TButton;
    Memo: TMemo;
    procedure ButtonInfoClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormMain: TFormMain;

implementation

uses Androidapi.Helpers, Androidapi.JNI.JavaTypes, Androidapi.JNIBridge,
  Androidapi.JNI.GraphicsContentViewText, android.net.wifi;

{$R *.fmx}

procedure TFormMain.ButtonInfoClick(Sender: TObject);
var
  Service: JObject;
  WifiManager: JWifiManager;
  ConnectionInfo: JWifiInfo;
  ScanResults: JList;
  ScanResult: JScanResult;
  I: Integer;
begin
  Memo.Lines.Clear;

  Service := SharedActivity.getSystemService(TJContext.JavaClass.WIFI_SERVICE);
  WifiManager := TJWifiManager.Wrap((Service as ILocalObject).GetObjectID);
  if not WifiManager.isWifiEnabled then
    Memo.Lines.Add('Wifi is disabled')
  else
  begin
    ConnectionInfo := WifiManager.getConnectionInfo;
    Memo.Lines.Add('Connection info');
    Memo.Lines.Add('  SSID: ' + JStringToString(ConnectionInfo.getSSID));
    Memo.Lines.Add('  BSSID: ' + JStringToString(ConnectionInfo.getBSSID));
    Memo.Lines.Add('  MAC address: ' + JStringToString(ConnectionInfo.getMacAddress));
    ScanResults := WifiManager.getScanResults;
    for I := 0 to ScanResults.size - 1 do
    begin
      Memo.Lines.Add('');
      Memo.Lines.Add('Detected access point ' + IntToStr(I));
      ScanResult := TJScanResult.Wrap((ScanResults.get(I) as ILocalObject).GetObjectID);
      Memo.Lines.Add('  SSID: ' + JStringToString(ScanResult.SSID));
      Memo.Lines.Add('  BSSID: ' + JStringToString(ScanResult.BSSID));
      Memo.Lines.Add('  Capabilities: ' + JStringToString(ScanResult.capabilities));
      Memo.Lines.Add('  Frequency: ' + IntToStr(ScanResult.frequency) + 'MHz');
      Memo.Lines.Add('  Signal level: ' + IntToStr(ScanResult.level) + 'dBm');
    end
  end
end;

end.
