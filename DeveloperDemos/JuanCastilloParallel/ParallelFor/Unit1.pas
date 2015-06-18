unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Memo;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
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

procedure TForm1.Button1Click(Sender: TObject);
begin
  Memo1.Lines.Clear;
  TParallel.For(1, 20,
    procedure(Index: Integer)
    begin
      Sleep(100);
      TThread.Queue(nil,
        procedure
        begin
          Memo1.Lines.Add(Index.ToString);
        end);
    end
  );
end;

end.
