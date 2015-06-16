
//---------------------------------------------------------------------------

// This software is Copyright (c) 2012 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit BitmapCodecForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.StdCtrls, FMX.Graphics;

type
  TBitmapCodecFrm = class(TForm)
    OpenDialog1: TOpenDialog;
    Button1: TButton;
    Button2: TButton;
    Image1: TImage;
    SaveDialog1: TSaveDialog;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  BitmapCodecFrm: TBitmapCodecFrm;

implementation

{$R *.fmx}

procedure TBitmapCodecFrm.Button1Click(Sender: TObject);
begin
  OpenDialog1.Filter := TBitmapCodecManager.GetFilterString;
  if OpenDialog1.Execute then 
    Image1.Bitmap.LoadFromFile(OpenDialog1.FileName);
end;

procedure TBitmapCodecFrm.Button2Click(Sender: TObject);
begin
  if SaveDialog1.Execute then
    Image1.Bitmap.SaveToFile(SaveDialog1.FileName);
end;

end.
