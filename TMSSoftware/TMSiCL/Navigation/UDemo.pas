unit UDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TMSNativeUILabel,
  FMX.TMSNativeMKMapView, FMX.TMSNativeUIImageView, FMX.TMSNativeUIButton,
  FMX.TMSNativeUIBaseControl, FMX.TMSNativeUIView,
  FMX.TMSNativeUIViewController, FMX.TMSNativeUINavigationController;

type
  TForm1034 = class(TForm)
    TMSFMXNativeUINavigationController1: TTMSFMXNativeUINavigationController;
    TMSFMXNativeUIButton1: TTMSFMXNativeUIButton;
    TMSFMXNativeUIViewController1: TTMSFMXNativeUIViewController;
    TMSFMXNativeUIImageView1: TTMSFMXNativeUIImageView;
    TMSFMXNativeUIButton2: TTMSFMXNativeUIButton;
    TMSFMXNativeUIViewController2: TTMSFMXNativeUIViewController;
    TMSFMXNativeUIButton3: TTMSFMXNativeUIButton;
    TMSFMXNativeMKMapView1: TTMSFMXNativeMKMapView;
    TMSFMXNativeUILabel1: TTMSFMXNativeUILabel;
    TMSFMXNativeUIButton4: TTMSFMXNativeUIButton;
    TMSFMXNativeUIButton5: TTMSFMXNativeUIButton;
    TMSFMXNativeUILabel2: TTMSFMXNativeUILabel;
    TMSFMXNativeUILabel3: TTMSFMXNativeUILabel;
    procedure TMSFMXNativeUIButton4Click(Sender: TObject);
    procedure TMSFMXNativeUIButton2Click(Sender: TObject);
    procedure TMSFMXNativeUIButton3Click(Sender: TObject);
    procedure TMSFMXNativeUIButton1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1034: TForm1034;

implementation

{$R *.fmx}

procedure TForm1034.TMSFMXNativeUIButton1Click(Sender: TObject);
begin
  TMSFMXNativeUINavigationController1.PushViewController(TMSFMXNativeUIViewController1, True);
end;

procedure TForm1034.TMSFMXNativeUIButton2Click(Sender: TObject);
begin
  TMSFMXNativeUINavigationController1.PushViewController(TMSFMXNativeUIViewController2, True);
end;

procedure TForm1034.TMSFMXNativeUIButton3Click(Sender: TObject);
begin
  ShowMessage('Finished');
end;

procedure TForm1034.TMSFMXNativeUIButton4Click(Sender: TObject);
begin
  TMSFMXNativeUINavigationController1.PopViewController(True);
end;

end.
