unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Media,
  FMX.Platform, FMX.Objects, FMX.Layouts, FMX.Memo;

type
  TCameraComponentForm = class(TForm)
    CameraComponent1: TCameraComponent;
    btnStartCamera: TButton;
    btnStopCamera: TButton;
    lblCameraType: TLabel;
    cbCameraFlashType: TLabel;
    imgCameraView: TImage;
    btnFrontCamera: TSpeedButton;
    btnBackCamera: TSpeedButton;
    btnOn: TSpeedButton;
    btnOff: TSpeedButton;
    btnAuto: TSpeedButton;
    procedure btnStartCameraClick(Sender: TObject);
    procedure btnStopCameraClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure CameraComponent1SampleBufferReady(Sender: TObject;
      const ATime: Int64);
    procedure btnFrontCameraClick(Sender: TObject);
    procedure btnBackCameraClick(Sender: TObject);
    procedure btnOnClick(Sender: TObject);
    procedure btnOffClick(Sender: TObject);
    procedure btnAutoClick(Sender: TObject);
  private
    { Private declarations }
    procedure GetImage;
  public
    { Public declarations }
    function AppEvent(AAppEvent: TApplicationEvent; AContext: TObject): Boolean;
  end;

var
  CameraComponentForm: TCameraComponentForm;

implementation

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}

procedure TCameraComponentForm.FormCreate(Sender: TObject);
var
  AppEventSvc: IFMXApplicationEventService;
begin
  { by default, we start with Front Camera and Flash Off }
{  cbFrontCamera.IsChecked := True;
   CameraComponent1.Kind := FMX.Media.TCameraKind.ckFrontCamera;

  cbFlashOff.IsChecked := True;
  if CameraComponent1.HasFlash then
    CameraComponent1.FlashMode := FMX.Media.TFlashMode.fmFlashOff;
}
  { Add platform service to see camera state. }
  if TPlatformServices.Current.SupportsPlatformService(IFMXApplicationEventService, IInterface(AppEventSvc)) then
    AppEventSvc.SetApplicationEventHandler(AppEvent);
end;

procedure TCameraComponentForm.Timer1Timer(Sender: TObject);
begin
  imgCameraView.Repaint;
end;

{ Make sure the camera is released if you're going away.}

function TCameraComponentForm.AppEvent(AAppEvent: TApplicationEvent;
  AContext: TObject): Boolean;
begin
  case AAppEvent of
    TApplicationEvent.WillBecomeInactive:
      CameraComponent1.Active := False;
    TApplicationEvent.EnteredBackground:
      CameraComponent1.Active := False;
    TApplicationEvent.WillTerminate:
      CameraComponent1.Active := False;
  end;
end;

procedure TCameraComponentForm.btnAutoClick(Sender: TObject);
begin
  { turn on automatic control of Flash, if supported }
  if CameraComponent1.HasFlash then
    CameraComponent1.FlashMode := FMX.Media.TFlashMode.AutoFlash;
end;

procedure TCameraComponentForm.btnBackCameraClick(Sender: TObject);
begin
  { select Back Camera }
  CameraComponent1.Active := False;
  CameraComponent1.Kind := FMX.Media.TCameraKind.BackCamera;
  CameraComponent1.Active := True;
end;

procedure TCameraComponentForm.btnFrontCameraClick(Sender: TObject);
begin
  { select Front Camera }
  CameraComponent1.Active := False;
  CameraComponent1.Kind := FMX.Media.TCameraKind.FrontCamera;
  CameraComponent1.Active := True;
end;

procedure TCameraComponentForm.btnOffClick(Sender: TObject);
begin
  { turn off Flash, if supported }
  if CameraComponent1.HasFlash then
    CameraComponent1.FlashMode := FMX.Media.TFlashMode.FlashOff;
end;

procedure TCameraComponentForm.btnOnClick(Sender: TObject);
begin
  { turn on Flash, if supported }
  if CameraComponent1.HasFlash then
    CameraComponent1.FlashMode := FMX.Media.TFlashMode.FlashOn;
end;

procedure TCameraComponentForm.btnStartCameraClick(Sender: TObject);
begin
  { turn on the Camera }
  CameraComponent1.Active := True;
end;

procedure TCameraComponentForm.btnStopCameraClick(Sender: TObject);
begin
  { turn off the Camera }
  CameraComponent1.Active := False;
end;

procedure TCameraComponentForm.CameraComponent1SampleBufferReady(
  Sender: TObject; const ATime: Int64);
begin
  TThread.Synchronize(TThread.CurrentThread, GetImage);
//  imgCameraView.Width := imgCameraView.Bitmap.Width;
//  imgCameraView.Height := imgCameraView.Bitmap.Height;
end;

procedure TCameraComponentForm.GetImage;
begin
  CameraComponent1.SampleBufferToBitmap(imgCameraView.Bitmap, True);
end;

end.
