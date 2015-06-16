unit UActivity;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.TMSNativeUIActivityViewController, FMX.TMSNativeUIBaseControl,
  FMX.TMSNativeUIButton, FMX.Objects, FMX.StdCtrls, FMX.TMSNativeUIImageView,
  FMX.TMSNativeUIView;

type
  TForm1073 = class(TForm)
    TMSFMXNativeUIButton1: TTMSFMXNativeUIButton;
    TMSFMXNativeUIActivityViewController1: TTMSFMXNativeUIActivityViewController;
    TMSFMXNativeUIView1: TTMSFMXNativeUIView;
    TMSFMXNativeUIImageView1: TTMSFMXNativeUIImageView;
    procedure TMSFMXNativeUIButton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1073: TForm1073;

implementation

{$R *.fmx}

procedure TForm1073.TMSFMXNativeUIButton1Click(Sender: TObject);
begin
  TMSFMXNativeUIActivityViewController1.Values.Add('Hello World !');
  TMSFMXNativeUIActivityViewController1.Bitmaps.Add(TMSFMXNativeUIImageView1.Bitmap);
  TMSFMXNativeUIActivityViewController1.Show;
end;

end.
