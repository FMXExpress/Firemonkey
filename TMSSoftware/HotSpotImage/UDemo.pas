unit UDemo;

interface

uses
  SysUtils, Types, UITypes, Classes, Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.TMSBitmap,
  FMX.TMSHotSpotImage, FMX.TMSHotSpotImageEditor, FMX.TMSBaseControl,
  FMX.TMSBitmapContainer, FMX.StdCtrls;

type
  TForm676 = class(TForm)
    Panel1: TPanel;
    Label1: TLabel;
    Label2: TLabel;
    TMSFMXHotSpotImage1: TTMSFMXHotSpotImage;
    Label3: TLabel;
    Button1: TButton;
    TMSFMXHotSpotImageEditorDialog1: TTMSFMXHotSpotImageEditorDialog;
    TMSFMXBitmapContainer1: TTMSFMXBitmapContainer;
    procedure TMSFMXHotSpotImage1HotSpotClick(Sender: TObject;
      HotSpot: TTMSFMXHotSpot);
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form676: TForm676;

implementation

{$R *.fmx}

procedure TForm676.Button1Click(Sender: TObject);
begin
  TMSFMXHotSpotImageEditorDialog1.Execute;
end;

procedure TForm676.FormCreate(Sender: TObject);
begin
  TMSFMXHotSpotImage1.Bitmap.Assign(TMSFMXBitmapContainer1.Items[0].Bitmap);
  {$IFDEF TMSIOS}
  Button1.Visible := False;
  {$ENDIF}
end;

procedure TForm676.TMSFMXHotSpotImage1HotSpotClick(Sender: TObject;
  HotSpot: TTMSFMXHotSpot);
begin
  Label2.Text := HotSpot.Name;
end;

end.
