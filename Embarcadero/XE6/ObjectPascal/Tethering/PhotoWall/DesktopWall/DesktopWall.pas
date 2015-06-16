unit DesktopWall;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Objects,
  IPPeerClient, IPPeerServer, System.Tether.Manager, System.Tether.AppProfile, System.Tether.NetworkAdapter;

type
  TForm48 = class(TForm)
    MediaReceiverManager: TTetheringManager;
    MediaReceiverProfile: TTetheringAppProfile;
    CalloutPanel1: TCalloutPanel;
    Image1: TImage;
    Label1: TLabel;
    procedure MediaReceiverProfileResourceReceived(const Sender: TObject; const AResource: TRemoteResource);
    procedure MediaReceiverManagerRequestManagerPassword(const Sender: TObject; const RemoteIdentifier: string; var Password: string);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form48: TForm48;

implementation

{$R *.fmx}

procedure TForm48.MediaReceiverManagerRequestManagerPassword(const Sender: TObject; const RemoteIdentifier: string; var Password: string);
begin
  Password := '1234';
end;

procedure TForm48.MediaReceiverProfileResourceReceived(const Sender: TObject; const AResource: TRemoteResource);
begin
  Image1.Bitmap.LoadFromStream(AResource.Value.AsStream);
end;

end.
