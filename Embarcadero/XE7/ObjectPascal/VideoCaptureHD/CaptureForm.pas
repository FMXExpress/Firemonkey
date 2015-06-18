
//---------------------------------------------------------------------------

// This software is Copyright (c) 2012 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit CaptureForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.Media, FMX.StdCtrls,
  FMX.Layouts, FMX.ListBox;

type
  TForm240 = class(TForm)
    Image1: TImage;
    CaptureButton: TSpeedButton;
    Layout1: TLayout;
    SaveDialog1: TSaveDialog;
    Ellipse1: TEllipse;
    StopButton: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure CaptureButtonClick(Sender: TObject);
    procedure StopButtonClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    VideoCamera: TVideoCaptureDevice;
    procedure SampleBufferReady(Sender: TObject; const ATime: TMediaTime);
  end;

var
  Form240: TForm240;

implementation

{$R *.fmx}
{$R *.Windows.fmx MSWINDOWS}

procedure TForm240.FormCreate(Sender: TObject);
begin
  VideoCamera := TCaptureDeviceManager.Current.DefaultVideoCaptureDevice;
  if VideoCamera <> nil then
  begin
    Ellipse1.AnimateFloat('Opacity', 1, 1.0);
    VideoCamera.OnSampleBufferReady := SampleBufferReady;
    VideoCamera.StartCapture;
  end
  else
  begin
    CaptureButton.Enabled := False;
    Caption := 'Video capture devices not available.';
  end;
end;

procedure TForm240.FormDestroy(Sender: TObject);
begin
  if VideoCamera <> nil then
    VideoCamera.StopCapture;
end;

procedure TForm240.SampleBufferReady(Sender: TObject; const ATime: TMediaTime);
begin
  VideoCamera.SampleBufferToBitmap(Image1.Bitmap, True);
end;

procedure TForm240.StopButtonClick(Sender: TObject);
begin
  if VideoCamera <> nil then
  begin
    if VideoCamera.State = TCaptureDeviceState.Capturing then
    begin
      Ellipse1.AnimateFloat('Opacity', 0, 1.0);
      StopButton.Text := 'Capture';
      VideoCamera.StopCapture;
    end
    else
    begin
      Ellipse1.AnimateFloat('Opacity', 1, 1.0);
      StopButton.Text := 'Stop';
      VideoCamera.StartCapture
    end;
  end;
end;

procedure TForm240.CaptureButtonClick(Sender: TObject);
begin
  if SaveDialog1.Execute then
    Image1.Bitmap.SaveToFile(SaveDialog1.FileName);
end;

end.
