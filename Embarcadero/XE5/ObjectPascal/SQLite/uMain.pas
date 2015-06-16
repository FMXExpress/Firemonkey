unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, Data.DbxSqlite, Data.FMTBcd,
  System.Rtti, System.Bindings.Outputs, Fmx.Bind.Editors, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, Data.Bind.Components, Data.Bind.DBScope, Data.DB,
  Data.SqlExpr, FMX.StdCtrls, FMX.Layouts, FMX.ListBox, FMX.MobilePreview;

type
  TSQLiteForm = class(TForm)
    ToolBar1: TToolBar;
    ListBox1: TListBox;
    btnAdd: TButton;
    TaskList: TSQLConnection;
    BindingsList1: TBindingsList;
    SQLQueryInsert: TSQLQuery;
    SQLQueryDelete: TSQLQuery;
    SQLDataSetTask: TSQLDataSet;
    BindSourceDB1: TBindSourceDB;
    LinkFillControlToField1: TLinkFillControlToField;
    Label1: TLabel;
    btnDelete: TButton;
    procedure btnAddClick(Sender: TObject);
    procedure TaskListBeforeConnect(Sender: TObject);
    procedure TaskListAfterConnect(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
  private
    procedure OnIdle(Sender: TObject; var ADone: Boolean);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  SQLiteForm: TSQLiteForm;

implementation

{$R *.fmx}

uses
System.iOUtils;

procedure TSQLiteForm.btnAddClick(Sender: TObject);
var
  TaskName: String;
begin
  try
    if InputQuery('Enter New Task', 'Task', TaskName) and not (TaskName.Trim = '') then
    begin
      SQLQueryInsert.ParamByName('TaskName').AsString := TaskName;
      SQLQueryInsert.ExecSQL();
      SQLDataSetTask.Refresh;
      LinkFillControlToField1.BindList.FillList;
    end;
  except
    on e: Exception do
    begin
      SHowMessage(e.Message);
    end;
  end;
end;

procedure TSQLiteForm.btnDeleteClick(Sender: TObject);
var
  TaskName: String;
  LIndex: Integer;
begin
  TaskName := ListBox1.Selected.Text;
  try
    SQLQueryDelete.ParamByName('TaskName').AsString := TaskName;
    SQLQueryDelete.ExecSQL();
    SQLDataSetTask.Refresh;
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

procedure TSQLiteForm.FormCreate(Sender: TObject);
begin
  try
    // For unidirectional dataset, don't refill automatically when dataset is activated
    // because dataset is reactivated everytime use DataSet.First.
    LinkFillControlToField1.AutoActivate := False;
    LinkFillControlToField1.AutoFill := False;
    Application.OnIdle := OnIdle;
    TaskList.Connected := True;
    SQLDataSetTask.Active := True;
    LinkFillControlToField1.BindList.FillList;
  except
    on e: Exception do
    begin
      SHowMessage(e.Message);
    end;
  end;
end;

procedure TSQLiteForm.OnIdle(Sender: TObject; var ADone: Boolean);
begin
  btnDelete.Visible := ListBox1.Selected <> nil;
end;

procedure TSQLiteForm.TaskListAfterConnect(Sender: TObject);
begin
  TaskList.ExecuteDirect('CREATE TABLE IF NOT EXISTS Task (TaskName TEXT NOT NULL)');
end;

procedure TSQLiteForm.TaskListBeforeConnect(Sender: TObject);
begin
  {$IF DEFINED(IOS) or DEFINED(ANDROID)}
  TaskList.Params.Values['Database'] :=   TPath.GetDocumentsPath + PathDelim + 'tasks.s3db';
  {$ENDIF}
end;

end.
