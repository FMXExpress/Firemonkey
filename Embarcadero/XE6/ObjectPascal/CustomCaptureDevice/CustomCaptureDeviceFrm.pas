
//---------------------------------------------------------------------------

// This software is Copyright (c) 2012 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit CustomCaptureDeviceFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Media, FMX.Objects, FMX.StdCtrls;

type
  TForm272 = class(TForm)
    Image1: TImage;
    SpeedButton1: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    CustomVideoDevice: TVideoCaptureDevice;
    procedure SampleBufferReady(Sender: TObject; const ATime: TMediaTime);
  end;

var
  Form272: TForm272;

implementation

{$R *.fmx}

procedure TForm272.FormCreate(Sender: TObject);
begin
  CustomVideoDevice := TVideoCaptureDevice(TCaptureDeviceManager.Current.GetDevicesByName('My Video Generator'));
  if CustomVideoDevice <> nil then
  begin
    CustomVideoDevice.OnSampleBufferReady := SampleBufferReady;
    CustomVideoDevice.StartCapture;
  end
  else
  begin
    Caption := 'Custom video device not available.';
  end;
end;

procedure TForm272.FormDestroy(Sender: TObject);
begin
  if CustomVideoDevice <> nil then
    CustomVideoDevice.StopCapture;
end;

procedure TForm272.SampleBufferReady(Sender: TObject; const ATime: TMediaTime);
begin
  CustomVideoDevice.SampleBufferToBitmap(Image1.Bitmap, True);
end;

procedure TForm272.SpeedButton1Click(Sender: TObject);
begin
  if CustomVideoDevice <> nil then
  begin
    if CustomVideoDevice.State = TCaptureDeviceState.Capturing then
    begin
      SpeedButton1.Text := 'Start';
      CustomVideoDevice.StopCapture;
    end
    else
    begin
      SpeedButton1.Text := 'Stop';
      CustomVideoDevice.StartCapture;
    end;
  end;
end;

end.
