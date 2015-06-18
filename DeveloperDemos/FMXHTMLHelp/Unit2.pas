unit Unit2;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Rtti, System.Classes,
  System.Variants, FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.StdCtrls, FMX.Edit, FMX.Layouts, FMX.Memo, System.Actions, FMX.ActnList,
  Winapi.Windows;

type
  TForm2 = class(TForm)
    Button1: TButton;
    CheckBox1: TCheckBox;
    Memo1: TMemo;
    Edit1: TEdit;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation

uses FMX.Platform.Win, HTMLHelpFMXViewer;

{$R *.fmx}


procedure TForm2.Button1Click(Sender: TObject);
begin
 HTMLHelpFMXViewer.ShowHTMLHelpContents;
end;

procedure TForm2.FormCreate(Sender: TObject);
var
  Helpfile : string;
begin
   Helpfile := IncludeTrailingBackslash(ExtractfilePath(GetModuleName(0)))+'Help.chm';
   if not HTMLHelpFMXViewer.SetHTMLHelpFile(Helpfile) then
    ShowMessage('Help file could not be found!! at ' + Helpfile);

RegisterFormForHelp(Self);
end;

initialization

end.
