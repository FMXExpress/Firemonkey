unit screenGameOver;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects;

type
  TGameOverFrame = class(TFrame)
    Rectangle1: TRectangle;
    Label1: TLabel;
    ScoreLBL: TLabel;
    Button1: TButton;
    HighScoreLBL: TLabel;
  private
    { Private declarations }
  public
    { Public declarations }
  end;

implementation

{$R *.fmx}

end.
