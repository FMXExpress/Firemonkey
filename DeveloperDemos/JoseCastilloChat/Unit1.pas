unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, IPPeerClient,
  IPPeerServer, System.Tether.Manager, System.Tether.AppProfile, FMX.StdCtrls,
  FMX.Layouts, FMX.Memo, FMX.ListBox, FMX.Controls.Presentation, FMX.Edit;

type
  TForm1 = class(TForm)
    TetheringManager1: TTetheringManager;
    TetheringAppProfile1: TTetheringAppProfile;
    btnDescubrir: TButton;
    btnEmparejar: TButton;
    ListBox1: TListBox;
    ListBox4: TListBox;
    btnConectar: TButton;
    ComboBox1: TComboBox;
    Memo1: TMemo;
    edtMensaje: TEdit;
    btnEnviarMensaje: TButton;
    procedure TetheringManager1EndManagersDiscovery(const Sender: TObject;
      const ARemoteManagers: TTetheringManagerInfoList);
    procedure btnDescubrirClick(Sender: TObject);
    procedure btnEmparejarClick(Sender: TObject);
    procedure TetheringManager1EndProfilesDiscovery(const Sender: TObject;
      const ARemoteProfiles: TTetheringProfileInfoList);
    procedure btnConectarClick(Sender: TObject);
    procedure ComboBox1Change(Sender: TObject);
    procedure TetheringAppProfile1AcceptResource(const Sender: TObject;
      const AProfileId: string; const AResource: TCustomRemoteItem;
      var AcceptResource: Boolean);
    procedure TetheringAppProfile1ResourceReceived(const Sender: TObject;
      const AResource: TRemoteResource);
    procedure btnEnviarMensajeClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}
{$R *.NmXhdpiPh.fmx ANDROID}
{$R *.SmXhdpiPh.fmx ANDROID}

procedure TForm1.btnDescubrirClick(Sender: TObject);
begin
 TetheringManager1.DiscoverManagers;
end;

procedure TForm1.btnEmparejarClick(Sender: TObject);
begin
  TetheringManager1.PairManager(TetheringManager1.RemoteManagers.Items[ListBox1.ItemIndex]);
end;

procedure TForm1.btnConectarClick(Sender: TObject);
begin
 if TetheringAppProfile1.Connect(TetheringManager1.RemoteProfiles.Items[ListBox4.ItemIndex]) then
  ShowMessage('Conectado');


end;

procedure TForm1.btnEnviarMensajeClick(Sender: TObject);
begin
 TetheringAppProfile1.SendString(TetheringManager1.RemoteProfiles.Items[ListBox4.ItemIndex],'',edtMensaje.Text);
 memo1.Lines.Add('->' + edtMensaje.Text);
 edtMensaje.Text := '';
end;

procedure TForm1.ComboBox1Change(Sender: TObject);
begin
// TetheringManager1.Enabled := False;
// TetheringManager1.AllowedAdapters:= Trim(ComboBox1.Items.Strings[Combobox1.ItemIndex]);
// TetheringManager1.Enabled := True;
end;

procedure TForm1.TetheringAppProfile1AcceptResource(const Sender: TObject;
  const AProfileId: string; const AResource: TCustomRemoteItem;
  var AcceptResource: Boolean);
begin


 AcceptResource := True;



end;

procedure TForm1.TetheringAppProfile1ResourceReceived(const Sender: TObject;
  const AResource: TRemoteResource);
begin

 Memo1.Lines.Add('<-'+ AResource.Value.AsString );

end;

procedure TForm1.TetheringManager1EndManagersDiscovery(const Sender: TObject;
  const ARemoteManagers: TTetheringManagerInfoList);
var
  i: Integer;
begin
 for i := 0 to TetheringManager1.RemoteManagers.Count-1 do
  begin
    ListBox1.Items.Add( TetheringManager1.RemoteManagers.Items[i].ManagerIdentifier );
  end;
end;

procedure TForm1.TetheringManager1EndProfilesDiscovery(const Sender: TObject;
  const ARemoteProfiles: TTetheringProfileInfoList);
var
  i: Integer;
begin
 for i := 0 to ARemoteProfiles.Count-1 do
  ListBox4.Items.Add( ARemoteProfiles.Items[i].ProfileIdentifier );
end;

end.
