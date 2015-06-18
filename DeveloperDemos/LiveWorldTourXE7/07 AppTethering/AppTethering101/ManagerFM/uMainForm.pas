unit uMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  System.Actions, FMX.ActnList, FMX.Layouts, FMX.ListBox, FMX.Edit,
  IPPeerClient, IPPeerServer, System.Tether.Manager, System.Tether.AppProfile,
  FMX.Controls.Presentation;

type
  TForm1 = class(TForm)
    Edit1: TEdit;
    Button1: TButton;
    ListBox1: TListBox;
    ActionList1: TActionList;
    Action1: TAction;
    TetheringManager1: TTetheringManager;
    TetheringAppProfile1: TTetheringAppProfile;
    procedure Action1Execute(Sender: TObject);
    procedure TetheringAppProfile1ResourceReceived(const Sender: TObject;
      const AResource: TRemoteResource);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Action1Execute(Sender: TObject);
begin
  if Edit1.Text <> '' then
    ListBox1.Items.Add(Edit1.Text);
end;

procedure TForm1.TetheringAppProfile1ResourceReceived(const Sender: TObject;
  const AResource: TRemoteResource);
begin
  if AResource.ResType = TRemoteResourceType.Data then
    Edit1.Text := AResource.Value.AsString;
end;

end.
