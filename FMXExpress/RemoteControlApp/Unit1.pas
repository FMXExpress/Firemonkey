unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, IPPeerClient,
  IPPeerServer, FMX.Objects, FMX.Controls.Presentation, FMX.StdCtrls,
  System.Tether.Manager, System.Tether.AppProfile, FMX.Layouts;

type
  TForm1 = class(TForm)
    TetheringManager1: TTetheringManager;
    TetheringAppProfile1: TTetheringAppProfile;
    Button1: TButton;
    Image1: TImage;
    Label1: TLabel;
    Button3: TButton;
    Layout1: TLayout;
    ToolBar1: TToolBar;
    Button2: TButton;
    Button4: TButton;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure TetheringManager1PairedToRemote(const Sender: TObject;
      const AManagerInfo: TTetheringManagerInfo);
    procedure TetheringManager1PairedFromLocal(const Sender: TObject;
      const AManagerInfo: TTetheringManagerInfo);
    procedure Button2Click(Sender: TObject);
    procedure TetheringAppProfile1ResourceReceived(const Sender: TObject;
      const AResource: TRemoteResource);
    procedure Button3Click(Sender: TObject);
    procedure Image1MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure Button4Click(Sender: TObject);
    procedure Button2MouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
  private
    { Private declarations }
    NButton: TButton;
    function FindControlAtPoint(aParent: TControl; aPos: TPointF): TControl;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

type
  THackControl = type TControl;

{$R *.fmx}

function TForm1.FindControlAtPoint(aParent: TControl; aPos: TPointF): TControl;
var
  I: Integer;
  Control, ChildControl: TControl;
  S: String;
begin
  Result := nil;

  // Check all the child controls and find the one at the coordinates
  for I := aParent.Controls.Count - 1 downto 0 do
  begin
    Control := aParent.Controls[I];
    S := Control.ClassName;
    if Control.PointInObject(aPos.X, aPos.Y) then
    begin
      ChildControl := FindControlAtPoint(Control, aPos);
      if Assigned(ChildControl) and ChildControl.HitTest then
        Exit(ChildControl)
      else
        if Control.HitTest then
          Exit(Control);
    end;
  end;
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
TetheringManager1.AutoConnect;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
TButton(Sender).Visible := False;
end;

procedure TForm1.Button2MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
TButton(Sender).Visible := False;
// Update the screenshot
Button3Click(Sender);
end;

procedure TForm1.Button3Click(Sender: TObject);
var
MS: TMemoryStream;
begin
MS := TMemoryStream.Create;
Layout1.MakeScreenShot.SaveToStream(MS);
MS.Seek(0,0);
TetheringAppProfile1.SendStream(TetheringManager1.RemoteProfiles[0],'ScreenShot',MS);
MS.Free;
end;

procedure TForm1.Button4Click(Sender: TObject);
begin
 NButton := TButton.Create(Self);
 NButton.Position.X := 50;
 NButton.Position.Y := 200;
 NButton.Text := 'New';
 //NButton.OnClick := Button2Click;
 NButton.OnMouseDown := Button2MouseDown;
 NButton.Parent := Layout1;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
Caption := Format('App: %s', [TetheringManager1.Identifier]);
end;

procedure TForm1.Image1MouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
begin
TetheringAppProfile1.SendString(TetheringManager1.RemoteProfiles[0],'MouseClick',X.ToString+','+Y.ToString);
end;

procedure TForm1.TetheringAppProfile1ResourceReceived(const Sender: TObject;
  const AResource: TRemoteResource);
var
Control: TControl;
SL: TStringList;
X,Y: Single;
begin
if AResource.ResType=TRemoteResourceType.Data then
 begin
  SL := TStringList.Create;
  SL.Delimiter := ',';
  SL.DelimitedText := AResource.Value.AsString;
  X := SL[0].ToSingle;
  Y := SL[1].ToSingle;
  SL.Free;
  Control := FindControlAtPoint(Layout1, Layout1.LocalToAbsolute(PointF(X, Y)));
  if Assigned(Control) and (TControl(Control) <> Layout1) then
  begin
    THackControl(Control).MouseDown(TMouseButton.mbLeft, [], X, Y);
    Application.ProcessMessages;
   // THackControl(Control).MouseUp(TMouseButton.mbLeft, [], X, Y);
    //THackControl(Control).Click;

    //showmessage('test');
  end;

 end
else
 Image1.Bitmap.LoadFromStream(AResource.Value.AsStream);
end;

procedure TForm1.TetheringManager1PairedFromLocal(const Sender: TObject;
  const AManagerInfo: TTetheringManagerInfo);
begin
Label1.Text := Format('Connected : %s %s',
                        [AManagerInfo.ManagerIdentifier,
                         AManagerInfo.ManagerName]);
end;

procedure TForm1.TetheringManager1PairedToRemote(const Sender: TObject;
  const AManagerInfo: TTetheringManagerInfo);
begin
Label1.Text := Format('Connected : %s %s',
                        [AManagerInfo.ManagerIdentifier,
                         AManagerInfo.ManagerName]);
end;

end.
