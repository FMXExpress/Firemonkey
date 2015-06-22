unit Unit2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms3D, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.MaterialSources, FMX.Objects3D, FMX.Objects, FMX.Controls3D, FMX.Layers3D,
  IdBaseComponent, IdComponent, IdTCPConnection, IdTCPClient, IdHTTP,
  FMX.Effects, FMX.Filter.Effects, IdCookieManager, IdIOHandler,
  IdIOHandlerSocket, IdIOHandlerStack, IdSSL, IdSSLOpenSSL,
  IdZLibCompressorBase, IdCompressorZLib, IdIntercept, IdCompressionIntercept,
  FMX.Ani, AnonThread, System.Sensors, FMX.Sensors,
  FMX.Layouts, FMX.StdCtrls;

type
  TForm2 = class(TForm3D)
    Cone1: TCone;
    ColorMaterialSource1: TColorMaterialSource;
    IdHTTP: TIdHTTP;
    Timer1: TTimer;
    IdCompressionIntercept1: TIdCompressionIntercept;
    IdCompressorZLib1: TIdCompressorZLib;
    IdCookieManager1: TIdCookieManager;
    LocationSensor1: TLocationSensor;
    IdSSLIOHandlerSocketOpenSSL1: TIdSSLIOHandlerSocketOpenSSL;
    Plane1: TPlane;
    TextureMaterialSource1: TTextureMaterialSource;
    FloatAnimation1: TFloatAnimation;
    InvertEffect1: TInvertEffect;
    Light1: TLight;
    LightMaterialSource1: TLightMaterialSource;
    Layer3D1: TLayer3D;
    ScanLBL: TLabel;
    Timer2: TTimer;
    FlowLayout1: TFlowLayout;
    procedure Timer1Timer(Sender: TObject);
    procedure LocationSensor1LocationChanged(Sender: TObject; const OldLocation,
      NewLocation: TLocationCoord2D);
    procedure Timer2Timer(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    M: TMemoryStream;
    Working: Boolean;
    CanLocation: Boolean;
    procedure LoadMapImage(URL: String);
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure SaveBitmapWithEffect(const Image: TImage; const FileName: string);
var
  Child: TFmxObject;
  Effect: TEffect;
  R: TRectF;
  Temp: TBitmap;
begin
  Effect := nil;
  R := RectF(0, 0, Image.Bitmap.Width, Image.Bitmap.Height);
  Temp := TBitmap.Create(Image.Bitmap.Width, Image.Bitmap.Height);
  Temp.Canvas.BeginScene;
  Temp.Canvas.DrawBitmap(Image.Bitmap, R, R, 1);
  Temp.Canvas.EndScene;
  try
    if Effect <> nil then
      Effect.ProcessEffect(Temp.Canvas, Temp, 1);
    Temp.SaveToFile(FileName);
  finally
    Temp.Free;
  end;
end;

procedure TForm2.LocationSensor1LocationChanged(Sender: TObject;
  const OldLocation, NewLocation: TLocationCoord2D);
begin
if CanLocation then
 begin
  CanLocation := False;
  // the larger the zoom number the closer it is
  LoadMapImage('http://maps.googleapis.com/maps/api/staticmap?center='+FloatToStr(NewLocation.Latitude)+','+FloatToStr(NewLocation.Longitude)+'&zoom=13&size=800x800&maptype=roadmap&sensor=false');
  ScanLBL.Text := 'Scanning location...';
 end;
end;

procedure TForm2.Timer1Timer(Sender: TObject);
begin
Timer1.Enabled := False;

LoadMapImage('http://maps.googleapis.com/maps/api/staticmap?center=United+States&zoom=3&size=800x800&maptype=roadmap&sensor=false');
ScanLBL.Text := 'Scanning location...';
LocationSensor1.Active := True;

end;

procedure TForm2.Timer2Timer(Sender: TObject);
begin
if LocationSensor1.Active=True then
 begin
  CanLocation := True;
 end;
end;

procedure Tform2.LoadMapImage(URL: String);
var
FThread: TAnonymousThread<Boolean>;
begin
  if Working=False then
  begin
    Working := True;
    M := TMemoryStream.Create;
      FThread := TAnonymousThread<Boolean>.Create(
        function: Boolean
        begin
          // Runs in seperate thread
            IdHTTP.Get(URL,M);
          Result := True;
        end,
        procedure(AResult: Boolean)
        var
          Temp: TBitmap;
        begin
          // Runs in main thread
          // process the result from above
           try
              M.Seek(0,0);
              //Image1.Bitmap.LoadFromStream(M);
              //Form2.BeginUpdate;
              Temp := TBitmap.Create;
              Temp.LoadFromStream(M);
              InvertEffect1.ProcessEffect(Temp.Canvas, Temp, 1);
              TextureMaterialSource1.Texture.Canvas.BeginScene;
              TextureMaterialSource1.Texture.Canvas.DrawBitmap(Temp,RectF(0,0,800,800),RectF(0,0,800,800),1);
              TextureMaterialSource1.Texture.Canvas.EndScene;
              {TextureMaterialSource1.Texture.Canvas.BeginScene;
              TextureMaterialSource1.Texture.Assign(Temp);
              TextureMaterialSource1.Texture.Canvas.EndScene; }
              ScanLBL.Text := 'Location acquired!';
              //InvalidateRect(RectF(0,0,Trunc(Layer3D1.Width),Trunc(Layer3D1.Height)));
              //Form2.EndUpdate;
              Temp.Free;
              M.Free;


           except on E: Exception do begin ShowMessage('Error: ' + E.Message); end;
           end;
           Working := False;
        end,
        procedure(AException: Exception)
        var
        II: Integer;
        begin
          // Runs in main thread
          // do something if there is an exception
           try
           M.Free;
           except on E: Exception do begin  end;
           end;
           ShowMessage('Error: ' + AException.Message);
           Working := False;
        end,
        False);
  end;
end;


end.
