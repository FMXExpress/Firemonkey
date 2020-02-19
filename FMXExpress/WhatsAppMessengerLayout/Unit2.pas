unit Unit2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.StdCtrls, FMX.Layouts, FMX.Controls.Presentation;

type
  TForm2 = class(TForm)
    VSB: TVertScrollBox;
    Layout1: TLayout;
    Button1: TButton;
    Button2: TButton;
    Image1: TImage;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    procedure LabelPaint(Sender: TObject; Canvas: TCanvas;
      const ARect: TRectF);
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.Button1Click(Sender: TObject);
var
CR: TCalloutRectangle;
L: TText;
TmpImg: TImage;
begin
  CR := TCalloutRectangle.Create(Self);
  CR.Parent := VSB;
  CR.Align := TAlignLayout.alTop;
  CR.CalloutPosition := TCalloutPosition.cpLeft;
  CR.Margins.Top := 10;
  CR.Margins.Bottom := 10;
  CR.Margins.Right := 5;
  CR.Height := 75;

  L := TText.Create(Self);
  L.Parent := CR;
  L.Align := TAlignLayout.alClient;
  L.Text := 'A quick brown fox jumped over the yellow log running away from the pink dog and ran down the lane.';
  L.Margins.Left := 15;
  L.Margins.Right := 5;
  L.Width := CR.Width-20;

  L.WordWrap := True;
  L.AutoSize := True;
  L.OnPaint := LabelPaint;

  TmpImg := TImage.Create(Self);
  TmpImg.Parent := CR;
  TmpImg.Align := TAlignLayout.alRight;
  TmpImg.Bitmap.Assign(Image1.Bitmap);
  TmpImg.Width := 75;
end;

procedure TForm2.Button2Click(Sender: TObject);
var
CR: TCalloutRectangle;
L: TText;
TmpImg: TImage;
begin
  CR := TCalloutRectangle.Create(Self);
  CR.Parent := VSB;
  CR.Align := TAlignLayout.alTop;
  CR.CalloutPosition := TCalloutPosition.cpRight;
  CR.Margins.Top := 10;
  CR.Margins.Bottom := 10;
  CR.Margins.Left := 5;
  CR.Height := 75;

  L := TText.Create(Self);
  L.Parent := CR;
  L.Align := TAlignLayout.alClient;
  L.Text := 'A quick brown fox jumped over the yellow log running away from the pink dog and ran down the lane.';
  L.Margins.Right := 15;
  L.Margins.Left := 5;
  L.Width := CR.Width-20;

  L.WordWrap := True;
  L.AutoSize := True;
  L.OnPaint := LabelPaint;

  TmpImg := TImage.Create(Self);
  TmpImg.Parent := CR;
  TmpImg.Align := TAlignLayout.alLeft;
  TmpImg.Bitmap.Assign(Image1.Bitmap);
  TmpImg.Width := 75;
end;

procedure TForm2.LabelPaint(Sender: TObject; Canvas: TCanvas;
  const ARect: TRectF);
begin
//TCalloutRectangle(TText(Sender).Parent).Height := TText(Sender).Height;
end;

end.
