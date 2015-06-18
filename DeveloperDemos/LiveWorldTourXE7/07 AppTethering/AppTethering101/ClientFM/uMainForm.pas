unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, IPPeerClient,
  IPPeerServer, System.Tether.Manager, System.Tether.AppProfile, FMX.StdCtrls,
  FMX.Edit, FMX.Controls.Presentation;

type
  TMainForm = class(TForm)
    ToolBar1: TToolBar;
    butPair: TButton;
    butAdd: TButton;
    TetheringManager1: TTetheringManager;
    TetheringAppProfile1: TTetheringAppProfile;
    Edit1: TEdit;
    butSend: TButton;
    procedure butPairClick(Sender: TObject);
    procedure TetheringManager1EndAutoConnect(Sender: TObject);
    procedure butAddClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

procedure TMainForm.butPairClick(Sender: TObject);
begin
  TetheringManager1.AutoConnect(5000);
end;

procedure TMainForm.butAddClick(Sender: TObject);
begin
  TetheringAppProfile1.RunRemoteAction(TetheringManager1.RemoteProfiles[0],
    'Action1');
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  TetheringAppProfile1.SendString(TetheringManager1.RemoteProfiles[0], 'Text',
    Edit1.Text);
end;

procedure TMainForm.TetheringManager1EndAutoConnect(Sender: TObject);
begin
  if TetheringManager1.RemoteProfiles.Count > 0 then
   begin
    butPair.Text := 'Connected...';
    butSend.Enabled := True;
    butAdd.Enabled := True;
   end;
end;

end.
