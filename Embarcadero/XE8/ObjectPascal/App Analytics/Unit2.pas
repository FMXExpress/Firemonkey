unit Unit2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation;

type
  TForm2 = class(TForm)
    ToolBar1: TToolBar;
    Label1: TLabel;
    Form2CloseButton: TButton;
    procedure Form2CloseButtonClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.fmx}

procedure TForm2.Form2CloseButtonClick(Sender: TObject);
begin
  Close;
end;

end.
