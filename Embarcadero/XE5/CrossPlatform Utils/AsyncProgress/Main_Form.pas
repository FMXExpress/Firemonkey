unit Main_Form;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls, FMX.Layouts,
  FMX.Memo;

type
  TMainForm = class(TForm)
    ExecuteButton: TButton;
    ExceptionSwitch: TSwitch;
    Label1: TLabel;
    ResultsMemo: TMemo;
    Layout1: TLayout;
    ToolBar1: TToolBar;
    Label2: TLabel;
    procedure ExecuteButtonClick(Sender: TObject);
  private
    { Private declarations }
    FCount: Integer;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

uses
  Xplat.Utils, FMX.Platform;

{$R *.fmx}

procedure TMainForm.ExecuteButtonClick(Sender: TObject);
begin
  TAsyncProgress<Boolean>.Execute(
    function: Boolean
    begin
      if ExceptionSwitch.IsChecked then
        raise Exception.Create('Danger Will Robinson!');
      Sleep(2000);
      Result := True;
    end,
    procedure(AResult: Boolean)
    begin
      Inc(FCount);
      ResultsMemo.Lines.Add(Format('Thread %d finished. Result=%s', [FCount, BoolToStr(AResult, True)]));
    end,
    procedure(AException: Exception)
    begin
      Inc(FCount);
      ResultsMemo.Lines.Add(Format('Thread %d - %s:%s', [FCount, AException.ClassName, AException.Message]));
    end);
end;

end.
