unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, Data.Bind.EngExt,
  Fmx.Bind.DBEngExt, Data.DBXInterbase, Data.FMTBcd, Data.SqlExpr, Data.DB,
  Data.Bind.Components, Data.Bind.DBScope, FMX.StdCtrls, FMX.ListView.Types,
  System.Rtti, System.Bindings.Outputs, Fmx.Bind.Editors, FMX.ListView;

type
  TIBLiteForm = class(TForm)
    BindingsList1: TBindingsList;
    BindSourceDB1: TBindSourceDB;
    TaskList: TSQLConnection;
    SQLQueryDelete: TSQLQuery;
    SQLQueryInsert: TSQLQuery;
    SQLDataSetTask: TSQLDataSet;
    ToolBar1: TToolBar;
    AddButton: TButton;
    DeleteButton: TButton;
    Label1: TLabel;
    ListView1: TListView;
    LinkFillControlToField1: TLinkFillControlToField;
    procedure AddButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TaskListBeforeConnect(Sender: TObject);
  private
    { Private declarations }
    procedure OnIdle(Sender: TObject; var ADone: Boolean);
  public
    { Public declarations }
  end;

var
  IBLiteForm: TIBLiteForm;

implementation

{$R *.fmx}

uses
System.iOUTils;

procedure TIBLiteForm.AddButtonClick(Sender: TObject);
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
      ShowMessage(e.Message);
    end;
  end;
end;

procedure TIBLiteForm.DeleteButtonClick(Sender: TObject);
var
  TaskName: string;
  LIndex: Integer;
begin
  TaskName := ListView1.Selected.Text;
  try
    SQLQueryDelete.ParamByName('TaskName').AsString := TaskName;
    SQLQueryDelete.ExecSQL;
    SQLDataSetTask.Refresh;
    LinkFillControlToField1.BindList.FillList;
    if (ListView1.Selected = nil) and (ListView1.Items.Count > 0) then
      // Select last item
      ListView1.ItemIndex := ListView1.Items.Count - 1;
  except
    on e: Exception do
    begin
      ShowMessage(e.Message);
    end;
  end;
end;

procedure TIBLiteForm.FormCreate(Sender: TObject);
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
      ShowMessage(e.Message);
    end;
  end;
end;


procedure TIBLiteForm.OnIdle(Sender: TObject; var ADone: Boolean);
begin
  DeleteButton.Visible := ListView1.Selected <> nil;
end;

procedure TIBLiteForm.TaskListBeforeConnect(Sender: TObject);
begin
  {$IF DEFINED(IOS) or DEFINED(ANDROID)}
  TaskList.Params.Values['Database'] := TPath.GetDocumentsPath + PathDelim + 'TASKS.GDB';
  {$ENDIF}
  TaskList.Params.Values['Username'] := 'sysdba';
  TaskList.Params.Values['Password'] := 'masterkey';
  TaskList.Params.Values['ServerCharSet'] := 'UTF8';
end;

end.
