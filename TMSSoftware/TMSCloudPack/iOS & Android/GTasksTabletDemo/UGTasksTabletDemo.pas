unit UGTasksTabletDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.TMSCloudBase, FMX.TMSCloudBaseFMX, FMX.TMSCloudCustomGoogle,
  FMX.TMSCloudGoogleFMX, FMX.TMSCloudCustomGTasks, FMX.TMSCloudGTasks,
  System.Rtti, FMX.Edit, FMX.DateTimeCtrls, FMX.StdCtrls, FMX.ListBox,
  FMX.Layouts, FMX.Grid, FMX.TMSCloudListView, IOUtils;

type
  TForm98 = class(TForm)
    TMSFMXCloudGTasks1: TTMSFMXCloudGTasks;
    lstvTaskLists: TTMSFMXCloudListView;
    lstTaskList: TListBox;
    Label1: TLabel;
    btnNextPage: TButton;
    Label2: TLabel;
    btnNextPageTasks: TButton;
    btnRemoveTaskList: TButton;
    btnRemoveTask: TButton;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    edListTitle: TEdit;
    edTitle: TEdit;
    cbCompleted: TCheckBox;
    dtDue: TCalendarEdit;
    lblLastUpdated: TLabel;
    lblTaskListId: TLabel;
    lblTaskId: TLabel;
    btnSaveTask: TButton;
    btnSaveList: TButton;
    btnConnect: TButton;
    btnAddTask: TButton;
    btnAddList: TButton;
    btnAddChildTask: TButton;
    Label9: TLabel;
    Label10: TLabel;
    procedure btnConnectClick(Sender: TObject);
    procedure TMSFMXCloudGTasks1ReceivedAccessToken(Sender: TObject);
    procedure btnSaveTaskClick(Sender: TObject);
    procedure lstTaskListClick(Sender: TObject);
    procedure btnNextPageClick(Sender: TObject);
    procedure btnRemoveTaskListClick(Sender: TObject);
    procedure btnAddListClick(Sender: TObject);
    procedure btnSaveListClick(Sender: TObject);
    procedure btnRemoveTaskClick(Sender: TObject);
    procedure btnAddTaskClick(Sender: TObject);
    procedure btnAddChildTaskClick(Sender: TObject);
    procedure btnNextPageTasksClick(Sender: TObject);
    procedure lstvTaskListsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure AddChildTask;
    procedure AddTask(parent: string = '');
    procedure AddTaskList;
    procedure ClearListFields;
    procedure ClearTaskFields;
    procedure EnableTaskListsElements;
    procedure EnableTasksElements;
    procedure GetNextPage;
    procedure GetNextTasksPage;
    procedure GetTaskLists;
    procedure GetTasks;
    procedure GetTasksList;
    procedure RemoveTask;
    procedure RemoveTaskList;
    procedure SaveTask;
    procedure SaveTaskList;
    procedure ShowTasks(item: TGTaskListItem);
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form98: TForm98;

implementation

{$R *.fmx}

{$I APPIDS.INC}

procedure TForm98.btnAddChildTaskClick(Sender: TObject);
begin
 AddChildTask();
end;

procedure TForm98.btnAddListClick(Sender: TObject);
begin
  AddTaskList();
end;

procedure TForm98.btnAddTaskClick(Sender: TObject);
begin
  AddTask();
end;

procedure TForm98.btnConnectClick(Sender: TObject);
begin
  TMSFMXCloudGTasks1.App.Key := GAppkey;
  TMSFMXCloudGTasks1.App.Secret := GAppSecret;

  TMSFMXCloudGTasks1.PersistTokens.Key := TPath.GetDocumentsPath + '/gtasks.ini';// --> IOS / Android
//  TMSFMXCloudGTasks1.PersistTokens.Key := ExtractFilePath(ParamStr(0)) + 'gtasks.ini';// --> Windows / Mac OSX
  TMSFMXCloudGTasks1.PersistTokens.Section := 'tokens';
  TMSFMXCloudGTasks1.LoadTokens;

  if TMSFMXCloudGTasks1.TestTokens then
  begin
    // Get the list
    GetTaskLists();
  end
  else
    TMSFMXCloudGTasks1.DoAuth;
end;

procedure TForm98.btnNextPageClick(Sender: TObject);
begin
  GetNextPage();
end;

procedure TForm98.btnNextPageTasksClick(Sender: TObject);
begin
  GetNextTasksPage();
end;

procedure TForm98.btnRemoveTaskClick(Sender: TObject);
begin
  RemoveTask();
end;

procedure TForm98.btnRemoveTaskListClick(Sender: TObject);
begin
  RemoveTaskList;
end;

procedure TForm98.btnSaveListClick(Sender: TObject);
begin
  SaveTaskList;
end;

procedure TForm98.btnSaveTaskClick(Sender: TObject);
begin
  SaveTask;
end;

// CLICK EVENTS


// EVENT METHODS
procedure TForm98.AddChildTask();
var
  parentId : string;
begin
  parentId := lblTaskId.Text;
  AddTask(parentId);
end;

procedure TForm98.AddTask( parent : string = '' );
var
  TaskListItem : TGTaskListItem;
  TaskListIndex: integer;
  TaskItem : TGTaskItem;
  Completed : string;
begin
  // Get all the stuff
  TaskListIndex := lstTaskList.ItemIndex;

  if (TaskListIndex >= 0)  then
  begin
    TaskListItem := TGTaskListItem(lstTaskList.Items.Objects[TaskListIndex]);

    // Set all the stuff
    TaskItem := TGTaskItem(TaskListItem.TaskItems.Add);
    TaskItem.Title := edTitle.Text;
    TaskItem.Due := dtDue.DateTime;
    TaskItem.Parent := parent;

    if cbCompleted.IsChecked then
    begin
      TaskItem.Status := 'completed';
      Completed := 'Yes';
    end
    else
    begin
      TaskItem.Status := 'needsAction';
      Completed := 'No';
    end;


    // Sent insert to api
    TaskListItem.AddTaskToList(TaskItem);

    TaskListItem.GetTaskItems;

    ShowTasks(TaskListItem);

    // Select the added item
//    lstvTaskLists.Items[0].MakeVisible(true);
    lstvTaskLists.SetFocus();
    //lstvTaskLists.ItemIndex := 0;
    lstvTaskLists.Selected := TListItem(lstTaskList.Items.Objects[TaskListIndex]);
  end;


end;

procedure TForm98.SaveTask;
var
  TaskListItem : TGTaskListItem;
  TaskListIndex: integer;
  TaskItem : TGTaskItem;
  TaskIndex: integer;
  Completed : string;
begin
  TaskListIndex := lstTaskList.ItemIndex;
  TaskIndex := lstvTaskLists.ItemIndex;
  TaskListItem := TGTaskListItem(lstTaskList.Items.Objects[TaskListIndex]);

  if TaskIndex >= 0 then
  begin
    TaskItem := TGTaskItem(lstvTaskLists.Items[TaskIndex].Data);

    // Set the values
    TaskItem.Title := edTitle.Text;
    edTitle.SetFocus();
    TaskItem.Due := dtDue.DateTime;

    if cbCompleted.IsChecked then
    begin
      TaskItem.Status := 'completed';
      Completed := 'Yes';
    end
    else
    begin
      TaskItem.Status := 'needsAction';
      Completed := 'No';
    end;

//    if dtDue.IsChecked = false then
//    begin
//      TaskItem.Due := Date;
//    end;

    TaskListItem.UpdateTask(TaskItem);

    // Add the task to the list

    // ListView
    lstvTaskLists.Items[TaskIndex].Text := TaskItem.Title;
    lstvTaskLists.Items[TaskIndex].SubItems[0] := DateToStr(TaskItem.Due);
    lstvTaskLists.Items[TaskIndex].SubItems[1] := Completed;
    lstvTaskLists.Items[TaskIndex].Data := TaskItem;
  end;
end;

procedure TForm98.RemoveTask;
var
  TaskListItem : TGTaskListItem;
  TaskListIndex: integer;
  TaskItem : TGTaskItem;
  TaskIndex: integer;
begin
  TaskListIndex := lstTaskList.ItemIndex;
  TaskIndex := lstvTaskLists.ItemIndex;

  if TaskIndex >= 0 then
  begin
    // Get the items
    TaskListItem := TGTaskListItem(lstTaskList.Items.Objects[TaskListIndex]);
    TaskItem := TGTaskItem(lstvTaskLists.Items[TaskIndex].Data);

    // Sent delete to api
    TaskListItem.DeleteTask(TaskItem);

    // remove from listview
    lstvTaskLists.Items.Delete(TaskIndex);


    // Go to previous selected item
    if (TaskIndex - 1 ) >= 0 then
    begin
      lstvTaskLists.ItemIndex := (TaskIndex -1 );

      // Scroll into view
      //lstvTaskLists.Items[(TaskIndex -1 )].MakeVisible(true);
    end
    else
    begin
    ClearTaskFields();
    btnRemoveTask.Enabled := false;
    btnSaveTask.Enabled := false;
    end;
  end;
end;

procedure TForm98.AddTaskList;
var
  TaskListItem : TGTaskListItem;
begin
  // Sent insert to api
  TaskListItem := TMSFMXCloudGTasks1.AddTaskList(edListTitle.Text);

  // Add listbox item
  lstTaskList.Items.AddObject(TaskListItem.Title, TaskListItem);

  lstTaskList.ItemIndex := lstTaskList.Items.Count - 1;
  lstTaskList.OnClick(Self);

  edListTitle.SetFocus();
end;

procedure TForm98.SaveTaskList;
var
  TaskListItem : TGTaskListItem;
  TaskListIndex: integer;
begin
  // Sent insert to api
  TaskListIndex := lstTaskList.ItemIndex;

  if TaskListIndex >= 0 then
  begin
    TaskListItem := TGTaskListItem(lstTaskList.Items.Objects[TaskListIndex]);
    TaskListItem.Title := edListTitle.Text;

    TMSFMXCloudGTasks1.UpdateTaskList(TaskListItem);

    // Update listbox item
    lstTaskList.Items[TaskListIndex] := edListTitle.Text;

    // Visuals
    lstTaskList.ItemIndex := TaskListIndex;
    lstTaskList.OnClick(Self);
    edListTitle.SetFocus();
  end
  else
  begin
    ClearListFields();
  end;
end;

procedure TForm98.RemoveTaskList;
var
  TaskListItem : TGTaskListItem;
  TaskListIndex: integer;
begin

  // Get Index & Build Object
  TaskListIndex := lstTaskList.ItemIndex;
  TaskListItem := TGTaskListItem(lstTaskList.Items.Objects[TaskListIndex]);

  // Delete
  TMSFMXCloudGTasks1.DeleteTaskList(TaskListItem);

  //Remove from list
  lstTaskList.Items.Delete(TaskListIndex);

  // Select another
  if (TaskListIndex -1) >= 0  then
  begin
    lstTaskList.ItemIndex := (TaskListIndex -1);
    lstTaskList.OnClick(Self)
  end;

end;

procedure TForm98.GetNextPage;
var
  List: TObject;
  Name : string;
  index: integer;
begin
  // Get the index
  index := lstTaskList.ItemIndex;

  // Fill it
  lstTaskList.Clear;
  TMSFMXCloudGTasks1.GetNextTaskListItems();

  // Show it
  for List in TMSFMXCloudGTasks1.TaskLists do
  begin
    Name := 'No name given...';
    if TGTaskListItem(List).Title <> '' then
      begin
        Name := TGTaskListItem(List).Title;
      end;

    lstTaskList.Items.AddObject(Name, List);
  end;

  // Make button visible if there's a next page
  btnNextPage.Enabled := TMSFMXCloudGTasks1.HasNextPage;

  // Select same item
  if index >= 0  then
  begin
    lstTaskList.ItemIndex := index;
    lstTaskList.OnClick(Self)
  end;

end;

procedure TForm98.GetNextTasksPage();
var
  Id : string;
  listIndex, taskIndex: integer;
  TaskListItem : TGTaskListItem;
begin
  // Get some stuff
  listIndex := lstTaskList.ItemIndex;
  taskIndex := lstvTaskLists.ItemIndex;
  TaskListItem := TGTaskListItem(lstTaskList.Items.Objects[listIndex]);
  Id := TaskListItem.Id;

  lstvTaskLists.Items.BeginUpdate;
  lstvTaskLists.Items.Clear;

  TaskListItem.GetNextTaskItems;

  // Fill the task list
  ShowTasks(TaskListItem);

  lstvTaskLists.Items.EndUpdate;

  // Visuals
  if (taskindex >= 0) then
  begin
    lstvTaskLists.ItemIndex := taskIndex;
    lstvTaskLists.Items[taskIndex].Selected := true;
  end;
end;

procedure TForm98.GetTasks;
var
  item: TGTaskListItem;
  selitem: integer;
begin
  // Visual Stuff...
  ClearTaskFields();
  EnableTaskListsElements();
  EnableTasksElements();

  // Get the list item
  selitem := lstTaskList.ItemIndex;

  if selitem >= 0 then
  begin
    btnRemoveTask.Enabled := false;
    btnSaveTask.Enabled := false;

    // Clear all previous tasks in the list
    lstvTaskLists.Items.Clear;

    if selitem >= 0 then
    begin
      item := TGTaskListItem(lstTaskList.Items.Objects[selitem]);

      // Set the list detail info
      edListTitle.Text := item.Title;
      lblTaskListId.Text :=  item.Id;
      lblLastUpdated.Text := DateTimeToStr(item.Updated);

      // Get all tasks
      item.GetTaskItems;

      // Fill the task list
      ShowTasks(item);
    end;

    // Visuals
    edListTitle.SetFocus();
  end
  else
  begin
    ClearListFields();
    ClearTaskFields();
  end;
end;

procedure TForm98.ShowTasks(item : TGTaskListItem);
var
  TaskItem: TObject;
  Completed, Child : string;
  Itm: TListItem;
  Name : string;
begin
  // First remove all items
  lstvTaskLists.Items.Clear;

  for TaskItem in item.TaskItems do
    begin
      // In case we have an empty name
      Name := 'No Taskname given';

      if TGTaskItem(TaskItem).Title <> '' then
      begin
        // Fill the name
        Name := TGTaskItem(TaskItem).Title;
      end;

      if TGTaskItem(TaskItem).Status = 'completed' then
      begin
        Completed := 'Yes';
      end
      else
      begin
        Completed := 'No';
      end;

      if TGTaskItem(TaskItem).Parent = '' then
      begin
        Child := 'Yes';
      end
      else
      begin
        Child := 'No';
      end;

      // ListView
      Itm := lstvTaskLists.Items.Add;
      Itm.Text := Name;
      Itm.SubItems.Add(DateTimeToStr(TGTaskItem(TaskItem).Due));
      Itm.SubItems.Add(Completed);
      Itm.SubItems.Add(Child);
      Itm.Data := TGTaskItem(TaskItem);

    end;

  // Make button visible if there's a next page
  if item.HasNextPage then
  begin
    btnNextPageTasks.Enabled := true;
  end
  else
  begin
    btnNextPageTasks.Enabled := false;
  end;
end;

procedure TForm98.TMSFMXCloudGTasks1ReceivedAccessToken(Sender: TObject);
begin
  //lstTaskList.Items.Add('got token');
  TMSFMXCloudGTasks1.SaveTokens;

  //TMSFMXCloudGTasks1.GetTaskListItems;
  GetTaskLists;
end;

procedure TForm98.GetTasksList;
var
  task: TGTaskItem;
  selitem: integer;
  myYear, myMonth, myDay : Word;
begin
  // Visual Stuff...
  EnableTasksElements();
  btnAddChildTask.Enabled := true;

  // Get the selected task object
  selitem := lstvTaskLists.ItemIndex;

  if selitem >= 0 then
  begin
    task := TGTaskItem(lstvTaskLists.Items[selitem].Data);

    if task <> nil then
    begin
      // Set the task details
      edTitle.SetFocus();
      lblTaskId.Text := task.Id;
      edTitle.Text := task.Title;

      DecodeDate(task.Due, myYear, myMonth, myDay);
      // 1970 = Default sql - no known - date
      if myYear > 1970 then
      begin
        dtDue.DateTime := task.Due;
//        dtDue.IsChecked := true;
      end;
//      else
//        dtDue.IsChecked := false;

      if task.Status = 'completed' then
      begin
        cbCompleted.IsChecked := true;
      end
      else
        cbCompleted.IsChecked := false;

     end;
  end
  else
  begin
    ClearTaskFields();
  end;
end;

procedure TForm98.lstTaskListClick(Sender: TObject);
begin
  GetTasks;
end;

procedure TForm98.lstvTaskListsClick(Sender: TObject);
begin
  GetTasksList;
end;

procedure TForm98.ClearTaskFields;
begin
  edTitle.Text := EmptyStr;
//  dtDue.IsChecked := false;
  dtDue.DateTime:= Date;
  cbCompleted.IsChecked := false;
  lblTaskId.Text := EmptyStr;
end;

procedure TForm98.ClearListFields;
begin
  edListTitle.Text := '';
  lblLastUpdated.Text := EmptyStr;
  lblTaskListId.Text := EmptyStr;
end;

procedure TForm98.EnableTaskListsElements;
begin
  edListTitle.Enabled := true;
  btnAddList.Enabled := true;
  btnSaveList.Enabled := true;
  btnRemoveTaskList.Enabled := true; // remove btn
end;

procedure TForm98.EnableTasksElements;
begin
  edTitle.Enabled := true;
  dtDue.Enabled := true;
  cbCompleted.Enabled := true;
  btnAddTask.Enabled := true; // add btn
  btnSaveTask.Enabled := true; // save btn
  btnRemoveTask.Enabled := true; // remove btn
end;



procedure TForm98.FormCreate(Sender: TObject);
begin
  edListTitle.Width := 369;
  lblLastUpdated.Width := 369;
  lblTaskListId.Width := 369;
  edTitle.Width := 369;
  dtDue.Width := 369;
  lblTaskId.Width := 369;
end;

procedure TForm98.GetTaskLists;
var
  List: TObject;
  Name : string;
begin
  lstTaskList.Items.Clear;
  ClearTaskFields;

  TMSFMXCloudGTasks1.GetTaskListItems;
  for List in TMSFMXCloudGTasks1.TaskLists do
  begin
    Name := 'No name given...';
    if TGTaskListItem(List).Title <> '' then
      begin
        Name := TGTaskListItem(List).Title;
      end;

    // listbox
    lstTaskList.Items.AddObject(Name, List);
  end;

//  btnNextPage.Enabled := TMSFMXCloudGTasks1.HasNextPage;
end;


end.
