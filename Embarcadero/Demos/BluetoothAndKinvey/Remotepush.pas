unit Remotepush;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, IPPeerClient,
  REST.Backend.PushTypes, REST.Backend.KinveyPushDevice, System.JSON,
  System.PushNotification, REST.OpenSSL, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit, FMX.EditBox, FMX.NumberBox,
  REST.Backend.KinveyProvider, Data.Bind.Components, Data.Bind.ObjectScope,
  REST.Backend.BindSource, REST.Backend.PushDevice, REST.Backend.MetaTypes,
  REST.Backend.KinveyServices, REST.Backend.ServiceComponents, System.Rtti,
  System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.EngExt, Fmx.Bind.DBEngExt,
  REST.Backend.ServiceTypes, REST.Backend.Providers, FMX.ListView.Types,
  FMX.ListView, FMX.ListBox, FMX.TabControl, FMX.Layouts, FMX.Objects;

type
  TForm30 = class(TForm)
    BackendPush1: TBackendPush;
    BindingsList1: TBindingsList;
    PushEvents1: TPushEvents;
    ToolBar1: TToolBar;
    Label1: TLabel;
    ListView1: TListView;
    Image1: TImage;
    procedure PushEvents1PushReceived(Sender: TObject; const AData: TPushData);
  private
    { Private declarations }
  public
    { Public declarations }

  end;

var
  Form30: TForm30;

implementation

{$R *.fmx}


procedure TForm30.PushEvents1PushReceived(Sender: TObject;
  const AData: TPushData);
begin
  ListView1.Items.Add.Text := AData.Message;
end;

end.
