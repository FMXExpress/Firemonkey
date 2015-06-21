unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Actions,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListView.Types, FMX.StdCtrls, FMX.ListView, FMX.ActnList, FMX.Menus,
  FMX.StdActns, FMX.MediaLibrary.Actions, Fmx.Bind.GenData,
  Data.Bind.GenData, System.Rtti, System.Bindings.Outputs, Fmx.Bind.Editors,
  Data.Bind.EngExt, Fmx.Bind.DBEngExt, Data.Bind.Components,
  Data.Bind.ObjectScope,
  PaxRunner, PaxProgram, PaxCompiler, FMX.Layouts,
  FMX.Memo, FMX.Controls.Presentation, ufrPlugins, PaxInterpreter;

type

  TfMain = class(TForm)
    PrototypeBindSource: TPrototypeBindSource;
    pxcmplr: TPaxCompiler;
    pxpsclng: TPaxPascalLanguage;
    btnRun: TButton;
    actlst: TActionList;
    actLoadPlugins: TAction;
    actEditCurrent: TAction;
    actAddPlugin: TAction;
    actSavePlugins: TAction;
    TakePhotoFromLibraryAction: TTakePhotoFromLibraryAction;
    actOpenUrl: TAction;
    frplgns: TfrPlugins;
    pxprgrm: TPaxInterpreter;
    procedure actOpenPluginExecute(Sender: TObject);
    procedure btnRunClick(Sender: TObject);
    procedure pxcmplrUnknownDirective(Sender: TPaxCompiler;
      const Directive: string; var ok: Boolean);
    procedure pxprgrmCreateObject(Sender: TPaxRunner; Instance: TObject);
  private
    { Private declarations }
    CurrModule: String;
  public
    { Public declarations }
  end;

var
  fMain: TfMain;
implementation
uses
    uPlugin,
    OpenViewUrl,
    PAXCOMP_SYS;
{$R *.fmx}
{------------------------------------------------------------------------------}
procedure TfMain.actOpenPluginExecute(Sender: TObject);
var
    plugin : TPlugin;
    P : Pointer;
begin
    plugin := frplgns.CurrentPlugin;
    if not Assigned(plugin) then
        exit;
    case plugin.FType of
        0: OpenViewUrl.OpenURL(plugin.FPath);
        1:
        begin
            pxcmplr.Reset;
            pxcmplr.RegisterLanguage(pxpsclng);
            pxcmplr.AddModule('1', 'Pascal');
            pxcmplr.AddCodeFromFile('1', plugin.FPath);
            if pxcmplr.Compile(pxprgrm) then
            begin
                P := pxprgrm.GetAddress('fMain');
                if Assigned(P) then
                    TfMain(P^) := Self; // change script-defind variable
                pxprgrm.Run;
            end
            else
                ShowMessage(pxcmplr.ErrorMessage[0]);
        end;
    end;
end;
{------------------------------------------------------------------------------}
procedure TfMain.btnRunClick(Sender: TObject);
begin
    actOpenPluginExecute(Sender);
end;
{------------------------------------------------------------------------------}
procedure TfMain.pxcmplrUnknownDirective(Sender: TPaxCompiler;
  const Directive: string; var ok: Boolean);
begin
      ok := true;
  CurrModule := Sender.CurrModuleName;
end;
{------------------------------------------------------------------------------}
procedure TfMain.pxprgrmCreateObject(Sender: TPaxRunner; Instance: TObject);
var
  P: PVmtMethodTable;
begin
  P := GetMethodTable(Instance.ClassType);
  if Instance is TForm then
    Sender.LoadDFMFile(Instance, CurrModule + '.dfm');
end;
{------------------------------------------------------------------------------}
end.
