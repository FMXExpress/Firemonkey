unit Login;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Edit, FMX.ListBox, FMX.Layouts, FMX.Objects,
  IPPeerClient, REST.Backend.ServiceTypes, REST.Backend.MetaTypes, System.JSON,
  REST.OpenSSL, REST.Backend.KinveyProvider, REST.Backend.Providers,
  REST.Backend.ServiceComponents, REST.Backend.KinveyServices, FMX.EditBox,
  FMX.NumberBox, FMX.TabControl;

type
  TloginForm = class(TForm)
    HeartRateCloudLoginImage: TImage;
    Loginlist: TListBox;
    Username: TEdit;
    Password: TEdit;
    ToolBar4: TToolBar;
    Label4: TLabel;
    ListBoxItem1: TListBoxItem;
    BackendUsers1: TBackendUsers;
    btnLogin: TButton;
    TabControl1: TTabControl;
    Threshold: TTabItem;
    Login: TTabItem;
    BPMNotificationThreshold: TListBoxGroupHeader;
    SetThresholdItem: TListBoxItem;
    SaveThreshold: TSpeedButton;
    BPMNotify: TNumberBox;
    ListBox1: TListBox;
    BackendStorage1: TBackendStorage;
    Image1: TImage;
    procedure btnLoginClick(Sender: TObject);
    procedure SaveThresholdClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  loginForm: TloginForm;

implementation

{$R *.fmx}

uses UHeartRateForm;

procedure TloginForm.btnLoginClick(Sender: TObject);
var
  ACreatedObject: TBackendEntityValue;
begin
    BackendUsers1.Users.LoginUser(Username.Text, Password.Text, ACreatedObject);
    ShowMessage('Logged in');
    TabControl1.ActiveTab := Threshold;
end;


procedure TloginForm.FormCreate(Sender: TObject);
begin
  TabControl1.ActiveTab := Login;
end;

procedure TloginForm.SaveThresholdClick(Sender: TObject);
var
  LJSON : TJSONObject;
  ACreatedObject: TBackendEntityValue;
begin
    LJSON := TJSONObject.Create;
    LJSON.AddPair('threshold', BPMNotify.Text);
    LJSON.AddPair('username', Username.Text);
    BackendStorage1.Storage.CreateObject('heartrateuserdata', LJSON, ACreatedObject);
    ShowMessage('Threshold set');
    loginForm.Hide;
    frmHeartMonitor.Show;
end;
end.{}




