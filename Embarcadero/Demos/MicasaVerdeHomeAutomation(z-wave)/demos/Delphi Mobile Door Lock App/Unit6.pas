unit Unit6;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.ListBox, FMX.Edit, FMX.Layouts, IPPeerClient, REST.Client,
  Data.Bind.Components, Data.Bind.ObjectScope;

type
  TForm6 = class(TForm)
    ToolBar1: TToolBar;
    Label1: TLabel;
    VeraLiteSwitch: TSwitch;
    ListBox1: TListBox;
    ListBoxItem1: TListBoxItem;
    VeraLiteEdit: TEdit;
    ListBoxItem2: TListBoxItem;
    ComboBox1: TComboBox;
    ListBoxItem3: TListBoxItem;
    DoItButton: TButton;
    RESTRequestLockOperation: TRESTRequest;
    RESTClient1: TRESTClient;
    procedure VeraLiteSwitchSwitch(Sender: TObject);
    procedure DoItButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form6: TForm6;

implementation

{$R *.fmx}

procedure TForm6.DoItButtonClick(Sender: TObject);
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

procedure TForm6.VeraLiteSwitchSwitch(Sender: TObject);
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
