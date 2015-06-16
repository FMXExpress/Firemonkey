
//---------------------------------------------------------------------------

// This software is Copyright (c) 2012 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit RectsFrm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.Objects, FMX.Graphics;

type
  TRectsForm = class(TForm)
    ScrollBox1: TScrollBox;
    Selection1: TSelection;
    Rectangle1: TRectangle;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  RectsForm: TRectsForm;

implementation

{$R *.fmx}

procedure TRectsForm.FormCreate(Sender: TObject);
const
  W = 50;
  H = 50;
var
  i, j: integer;
  R: TRectangle;
begin
  ScrollBox1.BeginUpdate;
  for i := 0 to 49 do
    for j := 0 to 49 do
    begin
      R := TRectangle.Create(nil);
      R.Parent := ScrollBox1;
      R.SetBounds(i * W + 2, j * H + 2, W - 4, H - 4);
      {$R-}
      R.Fill.Color := ((50 + random(205)) shl 24) or random($FFFFFF);
      {$R+}
      R.Stroke.Kind := TBrushKind.None;
      R.HitTest := False;
    end;
  ScrollBox1.EndUpdate;
end;

end.
