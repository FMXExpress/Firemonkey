unit UGTasksPhoneDemo;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.TMSCloudBase, FMX.TMSCloudBaseFMX, FMX.TMSCloudCustomGoogle,
  FMX.TMSCloudGoogleFMX, FMX.TMSCloudCustomGTasks, FMX.TMSCloudGTasks,
  System.Rtti, FMX.Edit, FMX.DateTimeCtrls, FMX.StdCtrls, FMX.ListBox,
  FMX.Layouts, FMX.Grid, FMX.TMSCloudListView, FMX.TabControl,
  System.Actions, FMX.ActnList, FMX.Objects, FMX.TMSCloudImage, IOUtils;

type
  TForm98 = class(TForm)
    TMSFMXCloudGTasks1: TTMSFMXCloudGTasks;
    tabLists: TTabItem;
    tabListDetail: TTabItem;
    lstTasks: TListBox;
    ListBoxHeader2: TListBoxHeader;
    Label1: TLabel;
    btnNextPageTasks: TButton;
    SearchBox2: TSearchBox;
    Label3: TLabel;
    edListTitle: TEdit;
    Label4: TLabel;
    lblLastUpdated: TLabel;
    lblTaskListId: TLabel;
    Label5: TLabel;
    btnRemoveTaskList: TButton;
    btnAddList: TButton;
    btnSaveList: TButton;
    TabControl2: TTabControl;
    tabAdd: TTabItem;
    tabShow: TTabItem;
    TabControl1: TTabControl;
    tabTaskDetail: TTabItem;
    speedbtn: TSpeedButton;
    lstTaskList: TListBox;
    ListBoxHeader1: TListBoxHeader;
    btnNextPage: TButton;
    SearchBox1: TSearchBox;
    btnConnect: TButton;
    btnAddTasks: TButton;
    Label6: TLabel;
    edTitle: TEdit;
    Label7: TLabel;
    dtDue: TCalendarEdit;
    cbCompleted: TCheckBox;
    Label8: TLabel;
    lblTaskId: TLabel;
    btnAddTask: TButton;
    btnAddChildTask: TButton;
    btnSaveTask: TButton;
    btnRemoveTask: TButton;
    tabChild: TTabItem;
    lblParent: TLabel;
    lblChildParent: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    dtChildDue: TCalendarEdit;
    cbChildCompleted: TCheckBox;
    edChildTitle: TEdit;
    Label12: TLabel;
    btnDoAddChild: TButton;
    Label2: TLabel;
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
    procedure btnTasksClick(Sender: TObject);
    procedure tabAddClick(Sender: TObject);
    procedure tabRemoveClick(Sender: TObject);
    procedure tabListsClick(Sender: TObject);
    procedure btnBackToListClick(Sender: TObject);
    procedure tabAddTaskClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure lstTaskListItemClick(const Sender: TCustomListBox;
      const Item: TListBoxItem);
    procedure speedbtnClick(Sender: TObject);
    procedure lstTasksClick(Sender: TObject);
    procedure lstTasksItemClick(const Sender: TCustomListBox;
      const Item: TListBoxItem);
    procedure btnAddTasksClick(Sender: TObject);
    procedure btnDoAddChildClick(Sender: TObject);
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
    procedure ClearChildFields;
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
  TabControl1.ActiveTab := tabChild;
  tabTaskDetail.Visible := false;
  lblChildParent.Text := lblTaskId.Text;
  tabChild.Visible := true;
  ClearChildFields();
end;

procedure TForm98.btnAddListClick(Sender: TObject);
begin
  AddTaskList();
end;

procedure TForm98.btnAddTaskClick(Sender: TObject);
begin
  AddTask();
end;

procedure TForm98.btnAddTasksClick(Sender: TObject);
begin        
  tabListDetail.Visible := false;
  tabTaskDetail.Visible := true;  
  TabControl1.ActiveTab := tabTaskDetail;
  btnSaveTask.Visible := false;
  btnAddChildTask.Visible := false;
  btnRemoveTask.Visible := false;
  ClearTaskFields; 
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

procedure TForm98.btnDoAddChildClick(Sender: TObject);
begin
  AddChildTask;
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
  {$if compilerversion > 26}
  MessageDlg('This will remove your Task. Are you sure?', TMsgDlgType.mtError, mbYesNo, 0,
  procedure(const AResult: TModalResult)
  begin
    if AResult = mrYes then
  {$endif}
      RemoveTask;
  {$if compilerversion > 26}
  end
  );
  {$endif}
end;

procedure TForm98.btnRemoveTaskListClick(Sender: TObject);
begin
  {$if compilerversion > 26}
  MessageDlg('This will remove your list. Are you sure?', TMsgDlgType.mtError, mbYesNo, 0,
  procedure(const AResult: TModalResult)
  begin
    if AResult = mrYes then
  {$endif}
      RemoveTaskList;
  {$if compilerversion > 26}
  end
  );
  {$endif}
end;

procedure TForm98.btnSaveListClick(Sender: TObject);
begin
  SaveTaskList;
end;

procedure TForm98.btnSaveTaskClick(Sender: TObject);
begin
  SaveTask;
end;

procedure TForm98.btnTasksClick(Sender: TObject);
begin
  tabTaskDetail.Enabled := true;
  TabControl1.ActiveTab := tabTaskDetail;
end;

procedure TForm98.btnBackToListClick(Sender: TObject);
begin
  tabLists.Visible := true;
  tabListDetail.Visible := false;
  TabControl1.ActiveTab := tabLists;
  TabControl2.ActiveTab := tabShow;
end;

// CLICK EVENTS


// EVENT METHODS
procedure TForm98.AddChildTask();
var
  parentId : string;
begin
  parentId := lblChildParent.Text;
  AddTask(parentId);
end;

procedure TForm98.AddTask( parent : string = '' );
var
  TaskListItem : TGTaskListItem;
  TaskIndex: integer;
  TaskItem : TGTaskItem;
  Completed : string;
begin
   if lstTaskList.ItemIndex >= 0 then
   begin;
      // Set all the stuff
      TaskIndex := lstTasks.ItemIndex;
      TaskListItem := TGTaskListItem(lstTaskList.Items.Objects[lstTaskList.ItemIndex]);

      if parent <> '' then
      begin
        TaskItem := TaskListItem.TaskItems.Add;
        TaskItem.Title := edChildTitle.Text;
        TaskItem.Due := dtChildDue.DateTime;
        TaskItem.Parent := parent;
      end
      else
      begin
        TaskItem := TaskListItem.TaskItems.Add;
        TaskItem.Title := edTitle.Text;
        TaskItem.Due := dtDue.DateTime;
        TaskItem.Parent := parent;
      end;


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

      if parent <> '' then
      begin
        lstTasks.ItemIndex := TaskIndex+1;
        tabChild.Visible := false;
        tabTaskDetail.Visible := true;
        TabControl1.ActiveTab := tabTaskDetail;
        lstTasks.OnItemClick(lstTasks, TListBoxItem(lstTasks.Items[TaskIndex+1]));
      end
      else
      begin
        lstTasks.ItemIndex := 0;
        lstTasks.OnItemClick(lstTasks, TListBoxItem(lstTasks.Items[0]));
      end;

      btnSaveTask.Visible := true;
      btnSaveTask.Enabled := true;
      btnAddChildTask.Visible := true;
      btnAddChildTask.Enabled := true;
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
  TaskIndex := lstTasks.ItemIndex;
  TaskListItem := TGTaskListItem(lstTaskList.Items.Objects[TaskListIndex]);

  if TaskIndex >= 0 then
  begin
    TaskItem := TGTaskItem(lstTasks.Items.Objects[TaskIndex]);

    // Set the values
    TaskItem.Title := edTitle.Text;
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

    TaskListItem.UpdateTask(TaskItem);
    
    // Listbox
    lstTasks.Items[TaskIndex] := TaskItem.Title;
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
  TaskIndex := lstTasks.ItemIndex;

  if TaskIndex >= 0 then
  begin
    // Get the items
    TaskListItem := TGTaskListItem(lstTaskList.Items.Objects[TaskListIndex]);
    TaskItem := TGTaskItem(lstTasks.Items.Objects[TaskIndex]);

    // Sent delete to api
    TaskListItem.DeleteTask(TaskItem);

    // remove from listview
    lstTasks.Items.Delete(TaskIndex);


    // Go to previous selected item
    if (TaskIndex - 1 ) >= 0 then
    begin
      lstTasks.ItemIndex := (TaskIndex -1 );
      lstTasks.OnItemClick(lstTasks, TListBoxItem(lstTasks.Items.objects[(TaskIndex-1)]));
    end
    else
    begin                  
    ClearTaskFields();
    btnRemoveTask.Enabled := false;
    btnSaveTask.Enabled := false;
    speedbtn.OnClick(self);
    end;
  end;
end;

procedure TForm98.AddTaskList;
var
  TaskListItem : TGTaskListItem;
  ListBoxItem : TListBoxItem;
begin
  lstTasks.Items.Clear;

  // Sent insert to api
  TaskListItem := TMSFMXCloudGTasks1.AddTaskList(edListTitle.Text);

  // Add listbox item
  ListBoxItem := TListBoxItem.Create(lstTaskList);
  ListBoxItem.ItemData.Accessory := TListBoxItemData.TAccessory(1);
  ListBoxItem.Data := TaskListItem;
  ListBoxItem.Text := TaskListItem.Title;
  lstTaskList.AddObject(ListBoxItem);
  //lstTaskList.Items.AddObject(TaskListItem.Title, TaskListItem);

  lblTaskListId.Text := TaskListItem.Id;
  lblLastUpdated.Text := DateTimeToStr(TaskListItem.Updated);
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

    // Update fields
    lblLastUpdated.Text := DateTimeToStr(TaskListItem.Updated);

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
    lstTaskList.OnItemClick(lstTaskList, TListBoxItem(lstTaskList.Items[(TaskListIndex -1)]));
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
  taskIndex := lstTasks.ItemIndex;
  TaskListItem := TGTaskListItem(lstTaskList.Items.Objects[listIndex]);
  Id := TaskListItem.Id;

  lstTasks.Items.BeginUpdate;
  lstTasks.Items.Clear;

  TaskListItem.GetNextTaskItems;

  // Fill the task list
  ShowTasks(TaskListItem);

  lstTasks.Items.EndUpdate;

  // Visuals
  if (taskindex >= 0) then
  begin
    lstTasks.ItemIndex := taskIndex;
  end;
end;

procedure TForm98.GetTasks;
var
  item: TGTaskListItem;
  selitem: integer;
begin

  // Get the list item
  selitem := lstTaskList.ItemIndex;

  if selitem >= 0 then
  begin
    // Visual Stuff...
    speedbtn.Visible := true;
    tabLists.Visible := false;
    tabListDetail.Visible := true;
    TabControl1.ActiveTab := tabListDetail;

    ClearTaskFields();
    EnableTaskListsElements();
    EnableTasksElements();

    // Clear all previous tasks in the list
    lstTasks.Items.Clear;

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
  Name : string;
  ListBoxItem : TListBoxItem;
begin
  // First remove all items
  lstTasks.Items.Clear;

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

      // listbox visuals
      // (aNone=0, aMore=1, aDetail=2, aCheckmark=3)
      ListBoxItem := TListBoxItem.Create(lstTasks);
      ListBoxItem.Text := Name;
      ListBoxItem.Data := TaskItem;
      ListBoxItem.ItemData.Accessory := TListBoxItemData.TAccessory(1);
      lstTasks.AddObject(ListBoxItem);

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

procedure TForm98.speedbtnClick(Sender: TObject);
begin
  if tabListDetail.Visible then
  begin
    tabLists.Visible := true;
    tabListDetail.Visible := false;      
    speedbtn.Visible := false;     
    TabControl1.ActiveTab := tabLists;
    TabControl2.ActiveTab := tabShow;
  end;

  if tabTaskDetail.Visible then
  begin
    TabControl1.ActiveTab := tabListDetail;
    tabTaskDetail.Visible := false;
    tabListDetail.Visible := true;
  end;

  if tabChild.Visible then
  begin
    TabControl1.ActiveTab := tabTaskDetail;
    tabTaskDetail.Visible := true;
    tabChild.Visible := false;
  end;
end;

procedure TForm98.tabAddClick(Sender: TObject);
begin
  // visual stuff
  speedbtn.Visible := true;
  tabLists.Visible := false;
  tabListDetail.Visible := true;
  ClearListFields();
  lstTasks.Items.Clear;

  // Show view for adding
  TabControl1.ActiveTab := tabListDetail;
end;

procedure TForm98.tabAddTaskClick(Sender: TObject);
begin

  TabControl1.ActiveTab := tabTaskDetail;
end;

procedure TForm98.tabListsClick(Sender: TObject);
begin
 TabControl2.ActiveTab := tabShow;
end;

procedure TForm98.tabRemoveClick(Sender: TObject);
begin
  RemoveTaskList;
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

  // Get the selected task object
  selitem := lstTasks.ItemIndex;

  if selitem >= 0 then
  begin
    // Visual Stuff...
    TabControl1.ActiveTab := tabTaskDetail;
    tabListDetail.Visible := false;
    tabChild.Visible := false;
    tabTaskDetail.Visible := true;

    EnableTasksElements();
    btnAddChildTask.Enabled := true;
    btnSaveTask.Enabled:= true;
    btnAddChildTask.Visible := true;
    btnSaveTask.Visible:= true;
    btnRemoveTask.Visible := true;

    task := TGTaskItem(lstTasks.Items.Objects[selitem]);

    if task <> nil then
    begin
      // Set the task details
      lblTaskId.Text := task.Id;
      edTitle.Text := task.Title;


      DecodeDate(task.Due, myYear, myMonth, myDay);
      // 1970 = Default sql - no known - date
      if myYear > 1970 then
      begin
        dtDue.DateTime := task.Due;
      end;

      if task.Status = 'completed' then
      begin
        cbCompleted.IsChecked := true;
      end
      else
        cbCompleted.IsChecked := false;
     end;

     if task.Parent <> '' then
       lblParent.Text := task.Parent
     else
       lblParent.Text := 'Yes';
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

procedure TForm98.lstTaskListItemClick(const Sender: TCustomListBox;
  const Item: TListBoxItem);
begin
  GetTasks;
end;

procedure TForm98.lstTasksClick(Sender: TObject);
begin
  GetTasksList;
end;

procedure TForm98.lstTasksItemClick(const Sender: TCustomListBox;
  const Item: TListBoxItem);
begin
  GetTasksList;
end;

procedure TForm98.lstvTaskListsClick(Sender: TObject);
begin
  GetTasksList;
end;

procedure TForm98.ClearChildFields;
begin
  edChildTitle.Text := '';
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
  speedbtn.Visible := false;
  tabListDetail.Visible := false;
  tabTaskDetail.Visible := false;
  tabChild.Visible := false;
  TabControl1.ActiveTab := tabLists;
end;

procedure TForm98.GetTaskLists;
var
  List: TObject;
  Name : string;
  ListBoxItem : TListBoxItem;
begin
  // visual stuff
  lstTaskList.Items.Clear;
  tabAdd.Enabled := true;
  ClearTaskFields;

  TMSFMXCloudGTasks1.GetTaskListItems;

  lstTaskList.BeginUpdate;
  for List in TMSFMXCloudGTasks1.TaskLists do
  begin
    Name := 'No name given...';
    if TGTaskListItem(List).Title <> '' then
      begin
        Name := TGTaskListItem(List).Title;
      end;

    // listbox visuals
    // (aNone=0, aMore=1, aDetail=2, aCheckmark=3)
    ListBoxItem := TListBoxItem.Create(lstTaskList);
    ListBoxItem.Text := Name;
    ListBoxItem.Data := List;
    ListBoxItem.ItemData.Accessory := TListBoxItemData.TAccessory(1);
    lstTaskList.AddObject(ListBoxItem);
  end;
  lstTaskList.EndUpdate;

  btnNextPage.Enabled := TMSFMXCloudGTasks1.HasNextPage;
end;


end.
