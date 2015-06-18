unit Unit6;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.Edit, FMX.StdCtrls,
  FMX.Objects, FMX.Layouts, FMX.ListBox, FMX.TabControl;

type
  TForm6 = class(TForm)
    Button2: TSpeedButton;
    Button3: TSpeedButton;
    Text1: TText;
    edtValue: TClearingEdit;
    Button1: TSpeedButton;
    ListBox1: TListBox;
    btnList: TSpeedButton;
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure btnListClick(Sender: TObject);
  private
    { Private declarations }
    function MyINIFilePath : string;
  public
    { Public declarations }
  end;

var
  Form6: TForm6;

implementation

{$R *.fmx}
uses INIFiles;

function TForm6.MyINIFilePath: string;
begin
//  Result := GetHomePath + PathDelim + 'Library' + PathDelim+'My.ini';
  Result := GetHomePath + PathDelim + 'Documents' + PathDelim+'MyD.ini';
end;

procedure TForm6.FormCreate(Sender: TObject);
begin
  // Show the Path on screen.
  Text1.Text := MyINIFilePath;
end;

procedure TForm6.btnListClick(Sender: TObject);
var
  Source : string;

  procedure DoListFiles(const Src:string);
  var
    SearchRec : TSearchRec;
    Res : Integer;
    PathFromRoot: string;
  begin
    Res := FindFirst(src + '*', faAnyFile, SearchRec);
    while Res = 0 do
    begin
      if (SearchRec.Attr and faDirectory) = faDirectory then
      begin
        if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
        begin
          // Do the recurse thing...
          DoListFiles(Src+SearchRec.Name+PathDelim);
        end;
      end
      else begin
        PathFromRoot := Copy(Src+SearchRec.Name,Length(GetHomePath+PathDelim),Length(Src+SearchRec.Name));
        ListBox1.Items.Add(PathFromRoot);
      end;
      Res := FindNext(SearchRec);
    end;
  end;
begin
  Source := GetHomePath+PathDelim;
  ListBox1.BeginUpdate;
  try
    ListBox1.Items.Clear;
    DoListFiles(Source);
  finally
    ListBox1.EndUpdate;
  end;
end;

procedure TForm6.Button1Click(Sender: TObject);
begin
  if FileExists(MyINIFilePath) then
    if DeleteFile(MyINIFilePath) then
      ShowMessage('Deleted')
    else
      ShowMessage('Not deleted');
end;

procedure TForm6.Button2Click(Sender: TObject);
var
  Ini : TINIFile;
begin
  Ini := Tinifile.Create(MyINIFilePath);
  try
    edtValue.Text := Ini.ReadString('Section','ID','');
  finally
    Ini.Free;
  end;
end;

procedure TForm6.Button3Click(Sender: TObject);
var
  Ini : TINIFile;
begin
  Ini := Tinifile.Create(MyINIFilePath);
  try
    Ini.WriteString('Section','ID',edtValue.Text);
  finally
    Ini.Free;
  end;
end;

end.
