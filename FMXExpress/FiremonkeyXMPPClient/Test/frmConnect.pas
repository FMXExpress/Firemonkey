unit frmConnect;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,

  fmxJabberClient, FMX.StdCtrls, IdIOHandler, IdIOHandlerSocket,
  IdIOHandlerStack, IdSSL, IdSSLOpenSSL, IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient, IdCoder, IdCoder3to4, IdCoderMIME, FMX.Edit,
  FMX.Layouts, FMX.ListBox,FMX.ListView,
  CommonHeader, FMX.Objects;

type
  TFormConnect = class(TForm)
    btnConnect: TButton;
    lbUsername: TLabel;
    edUsername: TEdit;
    lbPassword: TLabel;
    edPassword: TEdit;
    Label3: TLabel;
    BusyImage: TImage;
    OffLineImage: TImage;
    OnlineImage: TImage;
    MeImage: TImage;
    HimImage: TImage;
    procedure FormCreate(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
  private
    procedure OnLoggedProc;
    procedure OnGettingContactsProc;

  public
    { Déclarations publiques }
  end;

var
  FormConnect: TFormConnect;

implementation

{$R *.fmx}

uses FmxJabberTools, frmContacts;

procedure TFormConnect.btnConnectClick(Sender: TObject);
begin
  GJabberClient.Login := edUsername.Text;
  GJabberClient.Password := edPassword.Text;
  GJabberClient.JResource := 'OpenFireClient';
  GJabberClient.InitializeSocket;
  if not GJabberClient.Connected then
  begin
    Showmessage('Cannot connect to server !');
    Exit;
  end;
  GJabberClient.DoLogon;
  //Hide;
end;

procedure TFormConnect.FormCreate(Sender: TObject);
begin
  GJabberClient.JabberServer := ''; // put your server domain in here
  GJabberClient.JabberPort := 5222;
  GJabberClient.OnLogged := OnLoggedProc;
  GJabberClient.OnGettingContacts := OnGettingContactsProc;
end;

procedure TFormConnect.OnGettingContactsProc;
begin
  FormContacts.Showcontacts;
  FormContacts.cmbAvailabilityChange(FormContacts.cmbAvailability);
  FormContacts.lbDisplayName.Text := GJabberClient.Login;
  FormContacts.Show;
end;

procedure TFormConnect.OnLoggedProc;
begin
  GJabberClient.GetcontactsList;
end;



end.

