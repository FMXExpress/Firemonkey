
//---------------------------------------------------------------------------

// This software is Copyright (c) 2011 Embarcadero Technologies, Inc. 
// You may only use this software if you are an authorized licensee
// of Delphi, C++Builder or RAD Studio (Embarcadero Products).
// This software is considered a Redistributable as defined under
// the software license agreement that comes with the Embarcadero Products
// and is subject to that software license agreement.

//---------------------------------------------------------------------------
unit fMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, Generics.Collections,
  Data.Bind.Components, Data.Bind.EngExt, Fmx.Bind.DBEngExt, System.Rtti,
  System.Bindings.Outputs, FMX.Layouts, FMX.ListBox, FMX.Bind.Editors, FMX.Edit,
  FMX.StdCtrls;

type
  TMeeting = class
  private
    FStartTime: TDateTime;
    FDuration: Integer;
    FSubject: String;
    function GetEndTime: TDateTime;
    procedure SetSubject(const Value: String);
    procedure SetDuration(const Value: Integer);
    procedure SetStartTime(const Value: TDateTime);

  public
    constructor Create(const Subject : string; StartTime : TDateTime; Duration : Integer); virtual;
    property Subject: String read FSubject write SetSubject;
    property StartTime: TDateTime read FStartTime write SetStartTime;
    property Duration: Integer read FDuration write SetDuration;
    property EndTime: TDateTime read GetEndTime;
  end;

  TForm4 = class(TForm)
    BindScopeCollection: TBindScope;
    BindingsList1: TBindingsList;
    ListBox1: TListBox;
    bndlstMeetingsToListbox: TBindList;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    BindExprItemsSubject: TBindExprItems;
    BindExprItemsStart: TBindExprItems;
    BindExprItemsDuration: TBindExprItems;
    BindExprItemsEnd: TBindExprItems;
    Label5: TLabel;
    BindExprItemsLabel: TBindExprItems;
    BindScopeForm: TBindScope;
    BindExprItemsListBox: TBindExprItems;
    ListBox2: TListBox;
    Button1: TButton;
    TrackBar1: TTrackBar;
    BindExprItemsTrackBar11: TBindExprItems;
    CheckBoxLog: TCheckBox;
    CheckBoxNotify: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ListBox1Change(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
    procedure bndlstMeetingsToListboxAssignValue(Sender: TObject;
      AssignValueRec: TBindingAssignValueRec; var Value: TValue;
      var Handled: Boolean);
    procedure Button1Click(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
    procedure CheckBoxNotifyChange(Sender: TObject);
  private
    { Private declarations }
    MeetingList : TObjectList<TMeeting>;
    function GetCurrentMeeting: TMeeting;
  public
    { Public declarations }
    property CurrentMeeting: TMeeting read GetCurrentMeeting;
  end;

var
  Form4: TForm4;

implementation
uses
  DateUtils;

{$R *.fmx}

{ TMeeting }

constructor TMeeting.Create(const Subject: string; StartTime: TDateTime;
  Duration: Integer);
begin
  self.Subject := Subject;
  self.StartTime := StartTime;
  self.Duration := Duration;
end;

function TMeeting.GetEndTime: TDateTime;
begin
  Result := IncMinute(StartTime, Duration);
end;

procedure TMeeting.SetDuration(const Value: Integer);
begin
  FDuration := Value;
end;

procedure TMeeting.SetStartTime(const Value: TDateTime);
begin
  FStartTime := Value;
end;

procedure TMeeting.SetSubject(const Value: String);
begin
  FSubject := Value;
end;

procedure TForm4.bndlstMeetingsToListboxAssignValue(Sender: TObject;
  AssignValueRec: TBindingAssignValueRec; var Value: TValue;
  var Handled: Boolean);
var
  LObjClass: string;
  LObjName: string;
begin
  if not CheckBoxLog.IsChecked then
    Exit;

  if AssignValueRec.OutObj <> nil then
  begin
    LObjClass := AssignValueRec.OutObj.ClassName;
    if AssignValueRec.OutObj is TComponent then
      LObjName := TComponent(AssignValueRec.OutObj).Name;
  end
  else
  begin
    LObjClass := 'nil';
    Handled := True; // Don't raise exception when assign to nil
  end;

  ListBox2.Items.Add(Format('Assign Sender: %s, OutObj: %s: %s, OutProp: %s', [(Sender as TComponent).Name,
    LObjClass, LObjName, AssignValueRec.OutProp]));

end;

procedure TForm4.Button1Click(Sender: TObject);
begin
  ListBox2.Clear;
end;

procedure TForm4.CheckBoxNotifyChange(Sender: TObject);
var
  LComponent: TContainedBindComponent;
begin
  BindScopeForm.Active := False;
  for LComponent in BindingsList1 do
    if LComponent is TBindExprItems then
      TBindExprItems(LComponent).NotifyOutputs := CheckBoxNotify.IsChecked;
  BindScopeForm.Active := True;
end;

procedure TForm4.Edit1Change(Sender: TObject);
begin
  BindingsList1.Notify(Sender, 'Text');
end;

procedure TForm4.FormCreate(Sender: TObject);
begin
  MeetingList := TObjectList<TMeeting>.Create(True);
{$REGION 'Load MeetingList'}

    MeetingList.Add(TMeeting.Create('RAD Studio Product Address',
                                    EncodeDate(2011, 08, 10) + EncodeTime(9, 0, 0, 0),
                                    90));

    MeetingList.Add(TMeeting.Create('Coffee Break',
                                    EncodeDate(2011, 08, 10) + EncodeTime(10, 30, 0, 0),
                                    15));

    MeetingList.Add(TMeeting.Create('Deep Dive - FireMonkey',
                                    EncodeDate(2011, 08, 10) + EncodeTime(10, 45, 0, 0),
                                    45));

    MeetingList.Add(TMeeting.Create('Deep Dive - Delphi 64-bit, VCL Styles',
                                    EncodeDate(2011, 08, 10) + EncodeTime(11, 30, 0, 0),
                                    45));

    MeetingList.Add(TMeeting.Create('Lunch',
                                    EncodeDate(2011, 08, 10) + EncodeTime(12, 15, 0, 0),
                                    45));

    MeetingList.Add(TMeeting.Create('Deep Dive - LiveBindings',
                                    EncodeDate(2011, 08, 10) + EncodeTime(13, 0, 0, 0),
                                    45));

    MeetingList.Add(TMeeting.Create('Deep Dive - DataSnap and Cloud',
                                    EncodeDate(2011, 08, 10) + EncodeTime(13, 45, 0, 0),
                                    45));

    MeetingList.Add(TMeeting.Create('Coffee Break',
                                    EncodeDate(2011, 08, 10) + EncodeTime(14, 30, 0, 0),
                                    15));

    MeetingList.Add(TMeeting.Create('Deep Dive - Mobile and Connectors',
                                    EncodeDate(2011, 08, 10) + EncodeTime(14, 45, 0, 0),
                                    45));

    MeetingList.Add(TMeeting.Create('Summary and Q&A',
                                    EncodeDate(2011, 08, 10) + EncodeTime(15, 30, 0, 0),
                                    30));

{$ENDREGION}
  BindScopeCollection.DataObject := MeetingList;
  bndlstMeetingsToListbox.FillList;
  ListBox1.ItemIndex := 0;
  // Don't activate expressions until ready
  BindScopeForm.Active := True;
end;

procedure TForm4.FormDestroy(Sender: TObject);
begin
  MeetingList.Free;
end;

function TForm4.GetCurrentMeeting: TMeeting;
begin
  if ListBox1.ItemIndex >= 0 then
    Result := MeetingList[ListBox1.ItemIndex]
  else
    Result := nil;
end;

procedure TForm4.ListBox1Change(Sender: TObject);
begin
  // FMX sends two change notification, ignore the unselect
  if ListBox1.ItemIndex <> -1 then
    BindingsList1.Notify(Self, 'CurrentMeeting');
    ListBox2.Items.Add('------');
end;

procedure TForm4.TrackBar1Change(Sender: TObject);
begin
  BindingsList1.Notify(Sender, 'Value');
end;

end.
