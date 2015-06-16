unit UFaceDetection;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.TMSNativeUISwitch, FMX.TMSNativeUIView, FMX.TMSNativeUIImageView,
  FMX.TMSNativeUILabel, FMX.TMSNativeUIBaseControl, FMX.TMSNativeUIToolBar,
  FMX.TMSNativeUIImagePickerController, iOSApi.UIKit;

type
  TForm1136 = class(TForm)
    TMSFMXNativeUIToolBar1: TTMSFMXNativeUIToolBar;
    TMSFMXNativeUIImageView1: TTMSFMXNativeUIImageView;
    TMSFMXNativeUIImagePickerController1: TTMSFMXNativeUIImagePickerController;
    TMSFMXNativeUIView1: TTMSFMXNativeUIView;
    TMSFMXNativeUIView2: TTMSFMXNativeUIView;
    TMSFMXNativeUILabel1: TTMSFMXNativeUILabel;
    TMSFMXNativeUISwitch1: TTMSFMXNativeUISwitch;
    TMSFMXNativeUILabel2: TTMSFMXNativeUILabel;
    procedure TMSFMXNativeUISwitch1ValueChanged(ASender: TObject;
      AValue: Boolean);
    procedure TMSFMXNativeUIToolBar1ItemClick(ASender: TObject;
      AItem: TTMSFMXNativeUIToolBarItem);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
    procedure DidFinishPickingImage(Sender: TObject; ABitmap: UIImage);
  public
    { Public declarations }

  end;

var
  Form1136: TForm1136;

implementation

{$R *.fmx}

procedure TForm1136.DidFinishPickingImage(Sender: TObject; ABitmap: UIImage);
begin
  TMSFMXNativeUIImageView1.ImageView.setImage(ABitmap);
  if TMSFMXNativeUISwitch1.Value then
    TMSFMXNativeUIImageView1.ShowFaces;
end;

procedure TForm1136.FormCreate(Sender: TObject);
begin
  TMSFMXNativeUIImagePickerController1.OnDidFinishPickingImage := DidFinishPickingImage;
end;

procedure TForm1136.TMSFMXNativeUISwitch1ValueChanged(ASender: TObject;
  AValue: Boolean);
begin
  if AValue then
    TMSFMXNativeUIImageView1.ShowFaces
  else
    TMSFMXNativeUIImageView1.Faces.Clear;
end;

procedure TForm1136.TMSFMXNativeUIToolBar1ItemClick(ASender: TObject;
  AItem: TTMSFMXNativeUIToolBarItem);
begin
  TMSFMXNativeUIImagePickerController1.Show;
end;

end.
