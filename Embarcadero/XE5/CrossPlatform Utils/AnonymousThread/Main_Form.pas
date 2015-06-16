unit Main_Form;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Layouts, FMX.Memo,
  FMX.StdCtrls, AnonThread;

type
  TMainForm = class(TForm)
    ExecuteButton: TButton;
    Label1: TLabel;
    ExceptionSwitch: TSwitch;
    ResultsMemo: TMemo;
    Layout1: TLayout;
    ToolBar1: TToolBar;
    Label2: TLabel;
    Layout2: TLayout;
    SuspendedSwitch: TSwitch;
    Label3: TLabel;
    StartButton: TButton;
    procedure ExecuteButtonClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure StartButtonClick(Sender: TObject);
  private
    { Private declarations }
    FCount: Integer;
    FThreadID: TThreadID;
    FThread: TAnonymousThread<Boolean>;
    function IsMainThread: Boolean;
  public
    { Public declarations }
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}


function TMainForm.IsMainThread: Boolean;
begin
  Result := TThread.CurrentThread.ThreadID = FThreadID;
end;

procedure TMainForm.StartButtonClick(Sender: TObject);
begin
  if Assigned(FThread) and SuspendedSwitch.IsChecked then
    FThread.Start;
end;

procedure TMainForm.ExecuteButtonClick(Sender: TObject);
begin
  FThread := TAnonymousThread<Boolean>.Create(
    function: Boolean
    begin
      if ExceptionSwitch.IsChecked then
        raise Exception.Create('Danger Will Robinson!');
      Sleep(2000);
      Result := IsMainThread;
    end,
    procedure(AResult: Boolean)
    begin
      Inc(FCount);
      ResultsMemo.Lines.Add(Format('Thread %d finished', [FCount]));
      ResultsMemo.Lines.Add(Format('Ran in Main thread=%s', [BoolToStr(AResult, True)]));
      ResultsMemo.Lines.Add(Format('IsMainThread=%s', [BoolToStr(IsMainThread, True)]));
    end,
    procedure(AException: Exception)
    begin
      Inc(FCount);
      ResultsMemo.Lines.Add(Format('Thread %d - %s:%s', [FCount, AException.ClassName, AException.Message]));
      ResultsMemo.Lines.Add(Format('IsMainThread=%s', [BoolToStr(IsMainThread, True)]));
    end,
    SuspendedSwitch.IsChecked);
end;

procedure TMainForm.FormShow(Sender: TObject);
begin
  FThreadID := TThread.CurrentThread.ThreadID;
end;

end.
