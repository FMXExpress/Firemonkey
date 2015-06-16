unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Helpers.Android,

  Androidapi.JNI.JavaTypes,

  DPF.Android.Common,
  DPF.Android.DPFUtils,
  DPF.Android.BaseControl,
  DPF.Android.JView,
  DPF.Android.JButton,
  DPF.Android.JToast, DPF.Android.JTextView, DPF.Android.JRelativeLayout;

type
  TFDeviceInfo = class( TForm )
    DPFJView1: TDPFJRelativeLayout;
    DPFJTextView1: TDPFJTextView;
    procedure FormShow( Sender: TObject );
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FDeviceInfo: TFDeviceInfo;

implementation

{$R *.fmx}

procedure TFDeviceInfo.FormShow( Sender: TObject );
var
  di: TDeviceInfo;

begin
  di := GetDeviceInfo;
  DPFJTextView1.Text.Add( 'DeviceID(IMEI)(*#06#): ' + di.DeviceID + #10#13 );
  DPFJTextView1.Text.Add( 'Line1Number(Phone Number): ' + di.Line1Number + #10#13 );
  DPFJTextView1.Text.Add( 'DeviceSoftwareVersion: ' + di.DeviceSoftwareVersion + #10#13 );
  DPFJTextView1.Text.Add( 'NetworkOperator: ' + di.NetworkOperator + #10#13 );
  DPFJTextView1.Text.Add( 'NetworkOperatorName: ' + di.NetworkOperatorName + #10#13 );
  DPFJTextView1.Text.Add( 'NetworkCountryIso: ' + di.NetworkCountryIso + #10#13 );
  DPFJTextView1.Text.Add( 'SimOperator: ' + di.SimOperator + #10#13 );
  DPFJTextView1.Text.Add( 'SimOperatorName: ' + di.SimOperatorName + #10#13 );
  DPFJTextView1.Text.Add( 'SimCountryIso: ' + di.SimCountryIso + #10#13 );
  DPFJTextView1.Text.Add( 'SimSerialNumber: ' + di.SimSerialNumber + #10#13 );
  DPFJTextView1.Text.Add( 'SubscriberId: ' + di.SubscriberId + #10#13 );
  DPFJTextView1.Text.Add( 'Wifi Mac Address: ' + GetWifiMacAddress + #10#13 );
  DPFJTextView1.Text.Add( 'IP Address: ' + GetIPAddress( true ) + #10#13 );

end;

end.
