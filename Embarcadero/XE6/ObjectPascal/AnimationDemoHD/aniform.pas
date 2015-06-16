//---------------------------------------------------------------------------

// This software is Copyright (c) 2013 Embarcadero Technologies, Inc.
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------

unit aniform;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Ani, FMX.Objects;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    AniIndicator1: TAniIndicator;
    AniIndicator2: TAniIndicator;
    AniIndicator3: TAniIndicator;
    Ellipse1: TEllipse;
    Image1: TImage;
    Path1: TPath;
    Rectangle1: TRectangle;
    Rectangle2: TRectangle;
    Rectangle3: TRectangle;
    RoundRect1: TRoundRect;
    Text1: TText;
    Text2: TText;
    Text3: TText;
    Text4: TText;
    Text5: TText;
    Text6: TText;
    Text7: TText;
    FloatAnimation1: TFloatAnimation;
    FloatAnimation2: TFloatAnimation;
    PathAnimation1: TPathAnimation;
    PathAnimation2: TPathAnimation;
    ColorAnimation1: TColorAnimation;
    BitmapAnimation1: TBitmapAnimation;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

end.
