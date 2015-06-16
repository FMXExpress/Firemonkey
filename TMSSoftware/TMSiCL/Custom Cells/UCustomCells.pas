unit UCustomCells;

interface

uses
  System.SysUtils, System.Types, System.UITypes, DateUtils, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.TMSNativeUIBaseControl,
  FMX.TMSNativeUITableView, FMX.TMSNativeUITableViewMail, iOSApi.Foundation,
  FMX.TMSNativeUITextView, FMX.TMSNativeUILabel, FMX.TMSNativeUIView,
  FMX.TMSNativeUIButton;

type
  TForm907 = class(TForm)
    TMSFMXNativeUITableViewMail1: TTMSFMXNativeUITableViewMail;
    TMSFMXNativeUIView1: TTMSFMXNativeUIView;
    TMSFMXNativeUILabel1: TTMSFMXNativeUILabel;
    TMSFMXNativeUILabel2: TTMSFMXNativeUILabel;
    lblTitle: TTMSFMXNativeUILabel;
    lblSender: TTMSFMXNativeUILabel;
    TMSFMXNativeUILabel3: TTMSFMXNativeUILabel;
    lblDesc: TTMSFMXNativeUITextView;
    TMSFMXNativeUIButton1: TTMSFMXNativeUIButton;
    TMSFMXNativeUIView2: TTMSFMXNativeUIView;
    procedure FormCreate(Sender: TObject);
    procedure TMSFMXNativeUIButton1Click(Sender: TObject);
    procedure TMSFMXNativeUITableViewMail1GetItemFilterText(Sender: TObject;
      ASection, ARow: Integer; var AText: string);
    procedure TMSFMXNativeUITableViewMail1ItemSelect(Sender: TObject; ASection,
      ARow: Integer);
  private
    activemit: TTMSFMXNativeUITableViewMailItem;
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form907: TForm907;

implementation

{$R *.fmx}

procedure TForm907.FormCreate(Sender: TObject);
var
  s: TTMSFMXNativeUITableViewMailSection;
  mit: TTMSFMXNativeUITableViewMailItem;
  I: Integer;
  senderlist: TStringList;
  titles: TStringList;
  descriptions: TStringList;
  str: String;
begin
  if IsIpad then
  begin
    TMSFMXNativeUITableViewMail1.DetailView := TMSFMXNativeUIView2;
    TMSFMXNativeUITableViewMail1.Align := TAlignLayout.alLeft;
    TMSFMXNativeUITableViewMail1.Width := 320;
    TMSFMXNativeUIView2.Visible := True;
  end;


  senderlist := TStringList.Create;
  senderlist.Add('Steve Balmer');
  senderlist.Add('Tim Cook');
  senderlist.Add('Anders Ohlsson');
  senderlist.Add('David I');
  senderlist.Add('Marco Cantu');
  senderlist.Add('John Porter');

  titles := TStringList.Create;
  titles.Add('Take a look at the new Rad Studio XE4 !');
  titles.Add('Appointment');
  titles.Add('Order confirmation');
  titles.Add('New event taking place');

  descriptions := TStringList.Create;
  str := 'RAD Studio is the app development suite for companies who need'+
  'to create true native apps for PCs, tablets, and smartphones and get them to market fast. '+
  'Manage one codebase, one team, and one schedule without sacrificing performance. True native apps give you more '+
  'control, tighter security, and a better user experience.';
  descriptions.Add(str);
  str := 'A remarkably slim design that still makes room for a larger display and a faster chip.'+
  ' Ultrafast wireless that doesn’t sacrifice battery life. And all-new headphones designed to s'+
  'ound great and fit comfortably. So much went into this iPhone. So you could get even more out of it.';
  descriptions.Add(str);
  str := 'Mail (also known as Mail.app or Apple Mail) is an email program included with Apple Inc.'+
  ' Mac OS X operating system. Originally developed by NeXT as NeXTMail, a part of their NeXTSTEP '+
  'operating system, it was adapted to become OS X Mail application following Apple acquisition of NeXT.';
  descriptions.Add(str);
  str := 'A business model describes the rationale of how an organization creates, delivers, and captures value[1] (economic, social, cultural, or other forms of value).'+
  ' The process of business model construction is part of business strategy';
  descriptions.Add(str);

  s := TTMSFMXNativeUITableViewMailSection(TMSFMXNativeUITableViewMail1.Sections.Add);
  TMSFMXNativeUITableViewMail1.BeginUpdate;
  for I := 0 to 99 do
  begin
    mit := TTMSFMXNativeUITableViewMailItem(s.Items.Add);
    mit.Date := Incday(Now, -Random(100));
    mit.Title := titles[Random(titles.Count)];
    mit.Description := descriptions[Random(descriptions.Count)];
    mit.Sender := senderlist[Random(senderlist.Count)];
    mit.Unread := I < 3;
    if not isIPad then
    begin
      mit.SubDetailView := TMSFMXNativeUIView1;
      mit.AccessoryType := atTableViewCellAccessoryDisclosureIndicator;
    end
    else
      mit.DetailView := TMSFMXNativeUIView1;
  end;
  TMSFMXNativeUITableViewMail1.EndUpdate;

  senderlist.Free;
  titles.Free;
  descriptions.Free;
end;

procedure TForm907.TMSFMXNativeUIButton1Click(Sender: TObject);
begin
  activemit.Unread := True;
  TMSFMXNativeUITableViewMail1.HideDetailView;
end;

procedure TForm907.TMSFMXNativeUITableViewMail1GetItemFilterText(
  Sender: TObject; ASection, ARow: Integer; var AText: string);
var
  mit: TTMSFMXNativeUITableViewMailItem;
begin
  mit := TMSFMXNativeUITableViewMail1.Sections[ASection].Items[ARow] as TTMSFMXNativeUITableViewMailItem;
  if Assigned(mit) then
    AText := mit.Sender + ' ' + mit.Title + ' ' + mit.Description;
end;

procedure TForm907.TMSFMXNativeUITableViewMail1ItemSelect(Sender: TObject;
  ASection, ARow: Integer);
begin
  TMSFMXNativeUIButton1.Visible := not TMSFMXNativeUITableViewMail1.IsFiltering;
  activemit := TMSFMXNativeUITableViewMail1.Sections[ASection].Items[ARow] as TTMSFMXNativeUITableViewMailItem;
  activemit.Unread := False;
  lblTitle.Text := activemit.Title;
  lblSender.Text := activemit.Sender;
  lblDesc.Text := activemit.Description;
end;

end.
