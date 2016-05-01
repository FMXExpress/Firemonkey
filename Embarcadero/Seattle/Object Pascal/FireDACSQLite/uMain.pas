unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, Data.FMTBcd,
  System.Rtti, System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, Data.Bind.Components, Data.Bind.DBScope, Data.DB,
  FMX.StdCtrls, FMX.Layouts, FMX.ListBox, FireDAC.Stan.Intf, FireDAC.Stan.Option,
  FireDAC.Stan.Param, FireDAC.Stan.Error, FireDAC.DatS, FireDAC.Phys.Intf, FireDAC.DApt.Intf,
  FireDAC.Stan.Async, FireDAC.DApt, FireDAC.UI.Intf, FireDAC.Stan.Def, FireDAC.Stan.Pool,
  FireDAC.Phys, FireDAC.Comp.Client, FireDAC.Comp.DataSet, FireDAC.Stan.ExprFuncs,
  FireDAC.FMXUI.Wait, FireDAC.Comp.UI, FireDAC.Phys.SQLite,
  FireDAC.Phys.SQLiteDef;

type
  TFireDAC_SQLiteForm = class(TForm)
    ToolBar1: TToolBar;
    ListBox1: TListBox;
    btnAdd: TButton;
    BindingsList1: TBindingsList;
    BindSourceDB1: TBindSourceDB;
    LinkFillControlToField1: TLinkFillControlToField;
    FDTableTask: TFDTable;
    FireTaskList: TFDConnection;
    FDQueryDelete: TFDQuery;
    FDQueryInsert: TFDQuery;
    FDPhysSQLiteDriverLink1: TFDPhysSQLiteDriverLink;
    FDGUIxWaitCursor1: TFDGUIxWaitCursor;
    Title: TLabel;
    btnDelete: TButton;
    procedure btnAddClick(Sender: TObject);
    procedure TaskListBeforeConnect(Sender: TObject);
    procedure TaskListAfterConnect(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure OnIdle(Sender: TObject; var FDone: Boolean);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  FireDAC_SQLiteForm: TFireDAC_SQLiteForm;

implementation

uses
  IOUtils;

{$R *.fmx}

procedure TFireDAC_SQLiteForm.btnAddClick(Sender: TObject);
var
  LDefaultValue, LPrompt, TaskName: string;
begin
  try
    LPrompt := 'Task';
    LDefaultValue := '';
    InputQuery('Enter New Task', LPrompt, LDefaultValue,
      procedure(const AResult: TModalResult; const AValues: array of string)
      begin
        if AResult = mrOk then
          TaskName := AValues[0]
        else
          TaskName := '';
        if not (TaskName.Trim = '') then
        begin
          FDQueryInsert.ParamByName('TaskName').AsString := TaskName;
          FDQueryInsert.ExecSQL();
          FDTableTask.Refresh;
          LinkFillControlToField1.BindList.FillList;
        end;
      end);
  except
    on e: Exception do
    begin
      SHowMessage(e.Message);
    end;
  end;
end;

procedure TFireDAC_SQLiteForm.btnDeleteClick(Sender: TObject);
var
  TaskName: string;
  LIndex: Integer;
begin
  TaskName := ListBox1.Selected.Text;
  try
    FDQueryDelete.ParamByName('TaskName').AsString := TaskName;
    FDQueryDelete.ExecSQL();
    FDTableTask.Refresh;
    LinkFillControlToField1.BindList.FillList;
    if (ListBox1.Selected = nil) and (ListBox1.Count > 0) then
      // Select last item
      ListBox1.ItemIndex := ListBox1.Count - 1;
  except
    on e: Exception do
    begin
      SHowMessage(e.Message);
    end;
  end;
end;

procedure TFireDAC_SQLiteForm.FormCreate(Sender: TObject);
begin
  try
    // For unidirectional dataset, don't refill automatically when dataset is activated
    // because dataset is reactivated everytime use DataSet.First.
    LinkFillControlToField1.AutoActivate := False;
    LinkFillControlToField1.AutoFill := False;
    Application.OnIdle := OnIdle;
    FireTaskList.Connected := True;
    FDTableTask.Active := True;
    LinkFillControlToField1.BindList.FillList;
  except
    on e: Exception do
    begin
      SHowMessage(e.Message);
    end;
  end;
end;

procedure TFireDAC_SQLiteForm.OnIdle(Sender: TObject; var FDone: Boolean);
begin
  btnDelete.Visible := ListBox1.Selected <> nil;
end;

procedure TFireDAC_SQLiteForm.TaskListAfterConnect(Sender: TObject);
begin
  FireTaskList.ExecSQL ('CREATE TABLE IF NOT EXISTS Task (TaskName TEXT NOT NULL)');
end;

procedure TFireDAC_SQLiteForm.TaskListBeforeConnect(Sender: TObject);
begin
  {$IF DEFINED(IOS) or DEFINED(ANDROID)}
  FireTaskList.Params.Values['Database'] :=
    TPath.GetDocumentsPath + PathDelim + 'tasks.s3db';
  // was: GetHomePath + PathDelim +
  //  'Documents' + PathDelim + 'tasks.s3db';
  {$ENDIF}
end;

  // better use:

end.
