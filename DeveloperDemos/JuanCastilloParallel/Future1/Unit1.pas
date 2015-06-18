unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  System.Threading, FMX.StdCtrls;

type
  TForm1 = class(TForm)
    btnGetFuture: TButton;
    btnObtenerFuturo: TButton;
    procedure btnGetFutureClick(Sender: TObject);
    procedure btnObtenerFuturoClick(Sender: TObject);
  private
    FCadenaFutura: IFuture<string>;
  public
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.btnGetFutureClick(Sender: TObject);
begin
  FCadenaFutura := TTask.Future<string>(
      function: string
      begin
        sleep(2000);
        Result := '¡Hola programadores de Latinoamérica! ' + Random(100).ToString;
      end
    );
end;

procedure TForm1.btnObtenerFuturoClick(Sender: TObject);
begin
  btnObtenerFuturo.Text := FCadenaFutura.Value;
end;

end.
