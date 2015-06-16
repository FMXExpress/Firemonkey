unit UDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TMSNativeUIBaseControl,
  FMX.TMSNativeUIView, iOSApi.CoreGraphics, iOSApi.UIKit, iOSApi.Foundation,
  FMX.TMSNativeUIPDFPageViewController, FMX.StdCtrls, FMX.TMSNativeUIViewController,
  FMX.TMSNativeUIPageViewController, FMX.TMSNativeUIButton,
  FMX.TMSNativeUIPDFViewController;

type
  TForm1027 = class(TForm)
    TMSFMXNativeUIPDFViewController1: TTMSFMXNativeUIPDFViewController;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1027: TForm1027;

implementation

{$R *.fmx}

procedure TForm1027.FormCreate(Sender: TObject);
begin
  TMSFMXNativeUIPDFViewController1.Location := ExtractFilePath(ParamStr(0)) + '/Sample.pdf';
end;

end.
