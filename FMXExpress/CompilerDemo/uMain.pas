unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  PaxCompiler, FMX.TMSBaseControl, FMX.TMSMemo, PaxRunner, PaxInterpreter,
  FMX.Layouts, FMX.Memo, FMX.TMSFindDialog, FMX.Objects, FMX.ScrollBox,
  FMX.Controls.Presentation, System.Generics.Collections;

type
  TForm1 = class(TForm)
    TMSFMXMemo1: TTMSFMXMemo;
    PaxPascalLanguage1: TPaxPascalLanguage;
    PaxCompiler1: TPaxCompiler;
    btnSYntaxCheck: TButton;
    PaxInterpreter1: TPaxInterpreter;
    mmoMsg: TMemo;
    procedure btnSYntaxCheckClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure mmoMsgDblClick(Sender: TObject);
  private
    { Private declarations }
    FErrorList : TList<Integer>;
    function CheckSyntax(ACode : string) : Boolean;
    procedure SelectLine(lNumber : Integer);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.btnSYntaxCheckClick(Sender: TObject);
var
    i : integer;
    lNumber : integer;
    aMsg : string;
begin
    mmoMsg.Lines.Clear;
    FErrorList.Clear;
    if CheckSyntax(TMSFMXMemo1.Lines.Text) then
        exit;
    lNumber := PaxCompiler1.ErrorLineNumber[0];
    SelectLine(lNumber);
    for I:=0 to PaxCompiler1.ErrorCount - 1 do
    begin
        lNumber := PaxCompiler1.ErrorLineNumber[i];
        FErrorList.Add(lNumber);
        aMsg := 'msg: '+PaxCompiler1.ErrorMessage[i] +
            ' ln: '+inttostr(lNumber);
        mmoMsg.Lines.Add(aMsg);
    end;
end;

function TForm1.CheckSyntax(ACode: string): Boolean;
begin
    Result := false;
    PaxCompiler1.Reset;
    PaxCompiler1.RegisterLanguage(PaxPascalLanguage1);
    PaxCompiler1.AddModule('1', 'Pascal');
    PaxCompiler1.AddCode('1', ACode);
    if PaxCompiler1.Compile(PaxInterpreter1) then
        exit(True);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
    FErrorList := TList<Integer>.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
    FreeAndNil(FErrorList);
end;

procedure TForm1.mmoMsgDblClick(Sender: TObject);
var
    lNumber : integer;
begin
    lNumber := mmoMsg.CaretPosition.Line;
    if FErrorList.Count > lNumber then
        SelectLine(FErrorList[lNumber]);
end;

procedure TForm1.SelectLine(lNumber: Integer);
var
    lineText, temp : string;
    posStart, i : integer;
begin
    TMSFMXMemo1.ClearSelection;
   TMSFMXMemo1.SetCursor(0,lNumber);
   TMSFMXMemo1.ActiveLine := lNumber;
end;

end.
