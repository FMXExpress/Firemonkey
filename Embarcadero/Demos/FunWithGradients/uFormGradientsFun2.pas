unit uFormGradientsFun2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Ani;

type
  TFormGradientsFun2 = class(TForm)
    Rectangle1: TRectangle;
    GradientAnimation1: TGradientAnimation;
    procedure Rectangle1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FormGradientsFun2: TFormGradientsFun2;

implementation

{$R *.fmx}

procedure TFormGradientsFun2.Rectangle1Click(Sender: TObject);
begin
  GradientAnimation1.Start;
end;

end.
