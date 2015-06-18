unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls;

type
  TForm1 = class(TForm)
    btnTask1: TButton;
    lblTask1: TLabel;
    AniIndicator1: TAniIndicator;
    procedure btnTask1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  System.Threading;

{$R *.fmx}

procedure TForm1.btnTask1Click(Sender: TObject);
begin
  lblTask1.Text := '--';
  TTask.Run(
    procedure
    begin
      Sleep(5000);
      TThread.Synchronize(nil,
        procedure
        begin
          lblTask1.Text := Random(50).ToString;
        end
      );
    end
  );
end;

end.
