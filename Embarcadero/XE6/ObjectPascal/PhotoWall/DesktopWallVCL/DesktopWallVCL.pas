unit DesktopWallVCL;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, IPPeerClient, IPPeerServer, System.Tether.Manager, System.Tether.AppProfile,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Menus;

type
  TForm8 = class(TForm)
    Label1: TLabel;
    VCLMediaReceiver: TTetheringManager;
    VCLMediaReceiverApp: TTetheringAppProfile;
    Panel1: TPanel;
    Image1: TImage;
    procedure VCLMediaReceiverRequestManagerPassword(const Sender: TObject; const RemoteIdentifier: string; var Password: string);
    procedure VCLMediaReceiverAppResourceReceived(const Sender: TObject; const AResource: TRemoteResource);
  private
    { Private declarations }
    procedure LoadPhoto(const AStream: TMemoryStream);
  public
    { Public declarations }
  end;

var
  Form8: TForm8;

implementation

uses
  System.IOUtils, PNGImage, JPEG;

{$R *.dfm}

procedure TForm8.LoadPhoto(const AStream: TMemoryStream);
var
  FileFormat: Byte;
  LTmp: string;
begin
  if AStream.Size > 10 then
  begin
    LTmp := TPath.GetTempFileName;
    AStream.Position := 1;
    AStream.Read(FileFormat, 1);
    case FileFormat of
      $50: LTmp := ChangeFileExt(LTmp, '.png');
      $4A: LTmp := ChangeFileExt(LTmp, '.jpg');
    end;
    AStream.SaveToFile(LTmp);
    try
      Image1.Picture.LoadFromFile(LTmp);
    finally
      TFile.Delete(Ltmp);
    end;
  end;
end;

procedure TForm8.VCLMediaReceiverAppResourceReceived(const Sender: TObject; const AResource: TRemoteResource);
begin
  LoadPhoto(AResource.Value.AsStream as TMemoryStream);
end;

procedure TForm8.VCLMediaReceiverRequestManagerPassword(const Sender: TObject; const RemoteIdentifier: string; var Password: string);
begin
  Password := '1234';
end;

end.
