unit frmConversation;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Memo, FmxJabberTools, System.Rtti, FMX.Grid,
  ConversationListComponent, FMX.ListBox, CommonHeader;

type
  TFrameConversation = class(TFrame)
    MemText2Send: TMemo;
    btnSend: TButton;
    SpeedButton1: TSpeedButton;
    Panel1: TPanel;
    procedure btnSendClick(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private

  public
    TalkComp : TConversationList;
    MessageTo : string;
    OnSendMessage : TOnMessageReceived;
    procedure Initialize( ATo : String);
  end;

implementation

{$R *.fmx}

uses frmContacts;

procedure TFrameConversation.btnSendClick(Sender: TObject);
begin
  if (Assigned(OnSendMessage)) and (Trim(MemText2Send.Text) <> '') then
  begin
    OnSendMessage(MessageTo, MemText2Send.Text);
    TalkComp.AddMessageFromPeople1(MemText2Send.Text);
    MemText2Send.Text := '';
    MemText2Send.SetFocus;
  end;
end;


procedure TFrameConversation.Initialize(ATo: String);
begin
  TalkComp := TConversationList.Create(Self,'Me', ATo);
  TalkComp.Parent := Panel1;
  TalkComp.Align := TAlignLayout.alClient;

  TalkComp.Position.X := 5;
  TalkComp.Position.Y := 40;
  TalkComp.width := width - 10;
  TalkComp.Height := 380;


  panel1.Anchors := [TAnchorKind.akleft, TAnchorKind.aktop, TAnchorKind.akright, TAnchorKind.akbottom];

end;

procedure TFrameConversation.SpeedButton1Click(Sender: TObject);
begin
  FormContacts.TabControl1.ActiveTab := FormContacts.TabItem1;
end;


end.
