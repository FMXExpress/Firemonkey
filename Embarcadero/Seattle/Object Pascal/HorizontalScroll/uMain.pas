unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Objects, FMX.Layouts,
  FMX.StdCtrls;

type
  THorizontalScrollForm = class(TForm)
    HorzScrollBox1: THorzScrollBox;
    Image2: TImage;
    Image1: TImage;
    ToolBar1: TToolBar;
    Label1: TLabel;
    Image3: TImage;
    Image4: TImage;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  HorizontalScrollForm: THorizontalScrollForm;

implementation

{$R *.fmx}
{$R *.LgXhdpiPh.fmx ANDROID}

end.
