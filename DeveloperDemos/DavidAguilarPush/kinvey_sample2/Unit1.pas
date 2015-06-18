unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, IPPeerClient,
  REST.OpenSSL, REST.Backend.KinveyProvider, REST.Backend.PushTypes,
  System.JSON, REST.Backend.KinveyPushDevice, System.PushNotification,
  Data.Bind.Components, Data.Bind.ObjectScope, REST.Backend.BindSource,
  REST.Backend.PushDevice, FMX.StdCtrls, FMX.Layouts, FMX.ListBox;

type
  TForm1 = class(TForm)
    KinveyProvider1: TKinveyProvider;
    PushEvents1: TPushEvents;
    ToolBar1: TToolBar;
    ListBox1: TListBox;
    procedure PushEvents1DeviceTokenRequestFailed(Sender: TObject;
      const AErrorMessage: string);
    procedure PushEvents1DeviceTokenReceived(Sender: TObject);
    procedure PushEvents1PushReceived(Sender: TObject; const AData: TPushData);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.PushEvents1DeviceTokenReceived(Sender: TObject);
begin
  ListBox1.Items.Add('Token Recibido');
end;

procedure TForm1.PushEvents1DeviceTokenRequestFailed(Sender: TObject;
  const AErrorMessage: string);
begin
  ListBox1.Items.Add(AErrorMessage);
end;

procedure TForm1.PushEvents1PushReceived(Sender: TObject;
  const AData: TPushData);
begin
  ListBox1.Items.Add(AData.Message);
end;




end.
