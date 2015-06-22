unit unitMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  System.Generics.Collections, System.Threading, System.IOUtils,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.ListView.Types, FMX.Controls.Presentation, FMX.Edit, FMX.ListView,
  FMX.Layouts, FMX.Surfaces,
  FMX.Helpers.Android,
  Androidapi.Helpers,
  Androidapi.JNI.GraphicsContentViewText,
  Androidapi.JNI.JavaTypes,
  Androidapi.Jni,
  Androidapi.JNIBridge,
  Androidapi.JNI.App, FMX.StdCtrls;

type
  TformMain = class(TForm)
    layFind: TLayout;
    layAppList: TLayout;
    AppListView: TListView;
    txtFind: TEdit;
    ClearEditButton1: TClearEditButton;
    procedure FormCreate(Sender: TObject);
    procedure AppListViewItemClick(const Sender: TObject;
      const AItem: TListViewItem);
    procedure txtFindTyping(Sender: TObject);
    procedure txtFindChange(Sender: TObject);
  private
    { Private declarations }
    MainList : TList<JActivityInfo>;
    dictAppIcons : TDictionary<Integer, TBitmap>;
    procedure LoadActivityInfoList(var List : TList<JActivityInfo>);
    procedure LoadDictonaryAppIcons(index : Integer; appInfo : JApplicationInfo;
        var dictonaryAppIcons : TDictionary<Integer, TBitmap>);
    procedure LoadListView(listView : TListView; AppList: TList<JActivityInfo>;
        dictonaryAppIcons : TDictionary<Integer, TBitmap>);
    procedure OpenApp(PackageName, AppName : JString);
    procedure FilterListView(listView : TListView; filterName : string);
    procedure LoadListViewBitmap(listView: TListView; AppList: TList<JActivityInfo>;
        var dictonaryAppIcons : TDictionary<Integer, TBitmap>);
    function GetActivityAppList : JList;
    function GetOrSetCashAppIcon(appInfo : JApplicationInfo) : TBitmap;

  public
    { Public declarations }
  end;
const
    DEFAUT_INDEX : Integer = -1;
var
  formMain: TformMain;

implementation


{$R *.fmx}

{ TformMain }
{------------------------------------------------------------------------------}
procedure TformMain.OpenApp(PackageName, AppName : JString);
var
    Intent : JIntent;
    NativeComponent : JComponentName;
begin
    Intent := TJIntent.Create;
    Intent.setAction(TJIntent.JavaClass.ACTION_MAIN);
    Intent.addCategory(TJIntent.JavaClass.CATEGORY_LAUNCHER);
    NativeComponent := TJComponentName.JavaClass.init(PackageName, AppName);
    Intent.addFlags(TJIntent.JavaClass.FLAG_ACTIVITY_NEW_TASK or TJIntent.JavaClass.FLAG_ACTIVITY_RESET_TASK_IF_NEEDED);
    Intent.setComponent(NativeComponent);
    SharedActivity.startActivity(Intent);
end;
{------------------------------------------------------------------------------}
procedure TformMain.txtFindChange(Sender: TObject);
begin
  if txtFind.Text='' then
   FilterListView(self.AppListView,  txtFind.Text.Trim);
end;

procedure TformMain.txtFindTyping(Sender: TObject);
begin
    FilterListView(self.AppListView,  txtFind.Text.Trim);
end;
{------------------------------------------------------------------------------}
procedure TformMain.AppListViewItemClick(const Sender: TObject;
  const AItem: TListViewItem);
begin
    if not Assigned(MainList) then
        Exit;
    OpenApp(MainList.Items[AItem.Tag].applicationInfo.packageName,
        MainList.Items[AItem.Tag].name);
end;
{------------------------------------------------------------------------------}
procedure TformMain.FilterListView(listView : TListView; filterName : string);
var
    i : integer;
    item : TListViewItem;
    lower : string;
begin
    if not Assigned(listView) then
        exit;
    lower := filterName.ToLower.Trim;
    if lower.IsEmpty then
    begin
        if Assigned(listView.Items.Filter) then
            listView.Items.Filter := nil;
    end
    else
    begin
        listView.ItemIndex := DEFAUT_INDEX;
        listView.Items.Filter :=
            function(sFilter : string) : Boolean
            begin
                Result := (lower.IsEmpty) or sFilter.ToLower.Contains(lower);
            end;
    end;
end;
{------------------------------------------------------------------------------}
procedure TformMain.FormCreate(Sender: TObject);
begin
    LoadActivityInfoList(MainList);
    LoadListView(Self.AppListView, MainList, self.dictAppIcons);
    LoadListViewBitmap(Self.AppListView, MainList, self.dictAppIcons);
    Self.AppListView.So
end;
{------------------------------------------------------------------------------}
function TformMain.GetActivityAppList: JList;
var
    tempList : JList;
    Intent : JIntent;
    Manager : JPackageManager;
begin
    Intent := TJIntent.Create;
    Intent.setAction(TJIntent.JavaClass.ACTION_MAIN);
    Intent.addCategory(TJIntent.JavaClass.CATEGORY_LAUNCHER);
    Manager := SharedActivity.getPackageManager;
    tempList := nil;
    tempList := Manager.queryIntentActivities(Intent, 0);
    Result := tempList;
end;
{------------------------------------------------------------------------------}
function TformMain.GetOrSetCashAppIcon(appInfo: JApplicationInfo): TBitmap;
var
    Drawable : JDrawable;
    Bitmap : JBitmap;
    itemBitmap : TBitmap;
    Surface : TBitmapSurface;
    saveDir : string;
    pngFileName : string;
    SaveParams: TBitmapCodecSaveParams;
begin
    if not Assigned(appInfo) then
    begin
        Result := itemBitmap;
        exit;
    end;

    saveDir := TPath.GetCachePath;
    pngFileName := saveDir + '/' + JStringToString(appInfo.packageName) + '.png';
    itemBitmap := TBitmap.Create;
    if not TDirectory.Exists(saveDir, False) then
        TDirectory.CreateDirectory(saveDir);
     if TFile.Exists(pngFileName) then
        itemBitmap.LoadFromFile(pngFileName)
    else
    begin
        Drawable := appInfo.loadIcon(SharedActivity.getPackageManager);
        Bitmap := TJBitmapDrawable.Wrap((Drawable as ILocalObject).GetObjectID).getBitmap;
        Surface := TBitmapSurface.Create;
        try
            if JBitmapToSurface(Bitmap, Surface) then
                begin
                        itemBitmap.Assign(Surface);
                        SaveParams.Quality := 100;
                        itemBitmap.SaveToFile(pngFileName, @SaveParams);
                end;
        finally
            Surface.Free;
        end;
    end;
    Result := itemBitmap;
end;
{------------------------------------------------------------------------------}
procedure TformMain.LoadActivityInfoList(var List: TList<JActivityInfo>);
var
    tempList : JList;
    i : Integer;
    ResolveInfo : JResolveInfo;
    Info : JActivityInfo;
    AppInfo : JApplicationInfo;
begin
    if not Assigned(List) then
        List := TList<JActivityInfo>.Create;
    List.Clear;
    tempList := Self.GetActivityAppList;
    for i := 0 to tempList.size - 1 do
    begin
        ResolveInfo := TJResolveInfo.Wrap((tempList.get(i) as ILocalObject).GetObjectID);
        Info := TJActivityInfo.Wrap((ResolveInfo.activityInfo as ILocalObject).GetObjectID);
        AppInfo := TJApplicationInfo.Wrap((Info.applicationInfo as ILocalObject).GetObjectID);
        List.Add(Info);
    end;
end;
{------------------------------------------------------------------------------}
procedure TformMain.LoadDictonaryAppIcons(
  index : Integer; appInfo : JApplicationInfo; var dictonaryAppIcons : TDictionary<Integer, TBitmap>);
var
    itemBitmap : TBitmap;
begin
    if not Assigned(dictonaryAppIcons) then
        dictonaryAppIcons := TDictionary<Integer, TBitmap>.Create;
    if not dictonaryAppIcons.ContainsKey(index) then
    begin
        itemBitmap := GetOrSetCashAppIcon(appInfo);
        dictonaryAppIcons.AddOrSetValue(index, itemBitmap);
    end;
end;
{------------------------------------------------------------------------------}
procedure TformMain.LoadListView(listView: TListView; AppList: TList<JActivityInfo>;
    dictonaryAppIcons : TDictionary<Integer, TBitmap>);
var
    tempItem : TListViewItem;
    tempString, tempSubString, tempSubString2 : string;
    i : integer;
begin
    if (not Assigned(listView)) or (not Assigned(AppList)) then
        exit;
    listView.ClearItems;
    listView.BeginUpdate;
    for I := 0 to AppList.Count - 1 do
    begin
        tempString := JStringToString(AppList.Items[i].applicationInfo.loadLabel(SharedActivity.getPackageManager).toString);
        tempItem := listView.Items.Add;
        tempItem.Text := tempString;
        tempItem.Tag := i;
    end;
    listView.EndUpdate;
end;
{------------------------------------------------------------------------------}
procedure TformMain.LoadListViewBitmap(listView: TListView; AppList: TList<JActivityInfo>;
    var dictonaryAppIcons : TDictionary<Integer, TBitmap>);
var
    i : integer;
begin
    if (not Assigned(listView)) or (not Assigned(AppList)) then
        exit;
    listView.BeginUpdate;
    for I := 0 to listView.ItemCount - 1 do
    begin
        listView.Items[i].BeginUpdate;
        LoadDictonaryAppIcons(i, AppList.Items[listView.Items[i].Tag].applicationInfo, dictonaryAppIcons);
        if Assigned(dictonaryAppIcons) and (dictonaryAppIcons.ContainsKey(i)) then
            listView.Items[i].Bitmap := dictonaryAppIcons.Items[i];
        listView.Items[i].EndUpdate;
        Application.ProcessMessages;
    end;
    listView.EndUpdate;
end;
{------------------------------------------------------------------------------}
end.
