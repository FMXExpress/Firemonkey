unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, FMX.Objects, IdBaseComponent, IdComponent, IdTCPConnection,
  IdTCPClient, IdHTTP;

type
  TForm1 = class(TForm)
    IdHTTP: TIdHTTP;
    btnGetJPEG: TButton;
    imgCameraImage: TImage;
    procedure btnGetJPEGClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.btnGetJPEGClick(Sender: TObject);
var
  ResponseContent: TMemoryStream;
begin

  // Create response stream
  ResponseContent := TMemoryStream.Create;
  try

    // Request single JPEG from camera
    IdHTTP.Request.BasicAuthentication := TRUE;
    IdHTTP.Request.Username            := 'root';
    IdHTTP.Request.Password            := 'pass';
    IdHTTP.Get('http://10.1.39.2/axis-cgi/jpg/image.cgi', ResponseContent);

    // Load image to bitmap
    ResponseContent.Position := 0;
    imgCameraImage.Bitmap.LoadFromStream(ResponseContent);

  finally
    ResponseContent.Free;
  end;

end;

end.
