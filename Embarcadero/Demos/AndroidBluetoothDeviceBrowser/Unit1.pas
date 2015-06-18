unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  Androidapi.JNI.BluetoothAdapter,
  Androidapi.JNI.JavaTypes,
  Androidapi.JNIBridge,
  FMX.ListBox, FMX.Layouts, FMX.Memo, FMX.Edit, FMX.Objects, FMX.ListView.Types,
  FMX.ListView, System.Rtti, FMX.Grid, Data.Bind.GenData,
  System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, Data.Bind.Components, Data.Bind.ObjectScope;

type
  TForm1 = class(TForm)
    reload: TButton;
    Label1: TLabel;
    ListView1: TListView;
    procedure FormShow(Sender: TObject);
    procedure ListView1ItemClick(const Sender: TObject;
      const AItem: TListViewItem);
  private
    { private declarations }
  public
    { public declarations }
    targetMACAddress:string; // MAC address of selected device
    ostream:JOutputStream;
    istream:JInputstream;
    uid:JUUID;              // UUID for SPP traffic
    Sock:JBluetoothSocket;
    Adapter:JBluetoothAdapter;  // Local BLUETOOTH adapter
    remoteDevice:JBluetoothDevice;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormShow(Sender: TObject);
var
  s:string;
  i:integer;
  list:TStringList;
begin
  list:=TStringList.Create;
  s:=checkBluetooth; // Make sure bluetooth is enabled
  if pos('disabled',s)<>0 then begin
    ShowMessage('Please turn on Bluetooth :D and Retry');
    exit
  end;

  // This is the well known SPP UUID for connection to a Bluetooth serial device
  uid:=TJUUID.JavaClass.fromString(stringtojstring('00001101-0000-1000-8000-00805F9B34FB'));

  list.Clear;
  list.AddStrings(getbonded);    // produce a list of bonded/paired devices

  listview1.Items.Clear;  // clear list and rebuild it
  listview1.BeginUpdate;
  for i := 0 to list.Count-1 do begin
    listview1.Items.Add;
    listview1.Items.Item[i].Text:=list[i].Split(['='])[0];
    listview1.Items.Item[i].Detail:=list[i].Split(['='])[1];
  end;
  listview1.EndUpdate
end;

procedure TForm1.ListView1ItemClick(const Sender: TObject;
  const AItem: TListViewItem);
begin

  ShowMessage('You selected: '+Aitem.Text);

  // depending on the bluetooth device selected - do something with it
  targetMACAddress:=Aitem.Detail;
  if trim(targetMACAddress)='' then exit;

  Adapter:=TJBluetoothAdapter.JavaClass.getDefaultAdapter;
  remoteDevice:=Adapter.getRemoteDevice(stringtojstring(targetMACAddress));
  sock:=remoteDevice.createRfcommSocketToServiceRecord(UID);
  try
    sock.connect;
  except
    ShowMessage('Could not connect to BlueTooth device');
  end;
  if not sock.isConnected then
  begin
    ShowMessage('Failed to connect to Try again...');
    exit;
  end;
  listview1.Visible:=false;     // hide the chooser
  label1.Visible:=false;        // hide the chooser
  reload.Visible:=false;        // hide the chooser
end;

end.
