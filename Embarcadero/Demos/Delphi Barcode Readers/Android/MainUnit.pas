unit MainUnit;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.platform, fmx.helpers.android, FMX.Edit,
  FMX.StdCtrls,

  Android.BarcodeScanner, FMX.Layouts, FMX.Memo;

type
  TForm5 = class(TForm)
    Button1: TButton;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    abc: TAndroidBarcodeScanner;
    procedure DisplayBarcode(Sender: TAndroidBarcodeScanner; ABarcode: string);
  end;

var
  Form5: TForm5;

implementation

{$R *.fmx}


procedure TForm5.Button1Click(Sender: TObject);
begin

  abc.Scan;
end;

procedure TForm5.DisplayBarcode(Sender: TAndroidBarcodeScanner;
  ABarcode: string);
begin
  Memo1.Lines.Text := ABarcode;
end;

procedure TForm5.FormCreate(Sender: TObject);
begin


  abc := TAndroidBarcodeScanner.Create(true);
  abc.OnBarcode := DisplayBarcode;

end;

end.
