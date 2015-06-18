unit MainUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  IPPeerClient, REST.Client, Data.Bind.Components, Data.Bind.ObjectScope,
  FMX.Effects, FMX.Filter.Effects, FMX.ListBox, FMX.Edit;

type
  TForm1 = class(TForm)
    RESTClient1: TRESTClient;
    RESTRequestLockOperation: TRESTRequest;
    ComboBox1: TComboBox;
    Label1: TLabel;
    BandedSwirlEffect1: TBandedSwirlEffect;
    DoItButton: TButton;
    VeraLiteEdit: TEdit;
    Label2: TLabel;
    VeraLiteSwitch: TSwitch;
    procedure DoItButtonClick(Sender: TObject);
    procedure VeraLiteSwitchSwitch(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.DoItButtonClick(Sender: TObject);
var
  LockCommand : string;
begin
  // set the lock based on the combobox
  if ComboBox1.Items[ComboBox1.ItemIndex] = 'Unlock' then
    LockCommand := '0'
  else
    LockCommand := '1';
  RestRequestLockOperation.Resource :=
    'data_request?id=action&DeviceNum=3&serviceId=urn:micasaverde-com:serviceId:DoorLock1&action=SetTarget&newTargetValue='
    + LockCommand;
  RestRequestLockOperation.Execute
end;

procedure TForm1.VeraLiteSwitchSwitch(Sender: TObject);
begin
  // VeraLite Switch - check for operation
  if VeraLiteSwitch.IsChecked then begin
    // Set IP address and execute REST Request
    RESTClient1.BaseURL :=
      'http://'
      + VeraLiteEdit.Text
      + ':3480';
    DoItButton.Enabled := true
  end
  else
    DoItButton.Enabled := false
end;

end.
