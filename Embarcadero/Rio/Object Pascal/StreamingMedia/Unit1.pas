//---------------------------------------------------------------------------
// Copyright (c) 2016 Embarcadero Technologies, Inc. All rights reserved.  
//
// This software is the copyrighted property of Embarcadero Technologies, Inc. 
// ("Embarcadero") and its licensors. You may only use this software if you 
// are an authorized licensee of Delphi, C++Builder or RAD Studio 
// (the "Embarcadero Products").  This software is subject to Embarcadero's 
// standard software license and support agreement that accompanied your 
// purchase of the Embarcadero Products and is considered a Redistributable, 
// as such term is defined thereunder. Your use of this software constitutes 
// your acknowledgement of your agreement to the foregoing software license 
// and support agreement. 
//---------------------------------------------------------------------------
unit Unit1;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Media,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Edit, FMX.ScrollBox, FMX.Memo,
  FMX.TabControl, FMX.Layouts, FMX.ListBox, IdBaseComponent, IdComponent,
  IdTCPConnection, IdTCPClient, IdHTTP;

type
  TForm1 = class(TForm)
    Button1: TButton;
    MediaPlayer1: TMediaPlayer;
    Edit1: TEdit;
    Button2: TButton;
    ProgressBar1: TProgressBar;
    TabControl1: TTabControl;
    TabItem1: TTabItem;
    TabItem2: TTabItem;
    ListBox1: TListBox;
    MediaPlayerControl1: TMediaPlayerControl;
    ListBox2: TListBox;
    Button3: TButton;
    IdHTTP1: TIdHTTP;
    ToolBar1: TToolBar;
    Label1: TLabel;
    ToolBar2: TToolBar;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
    procedure ScanText(const AText: string);
    function GetTagValue(var AText: string): string;
    function IsItemCorrect(const AItem: string): Boolean;
    procedure AddItem(const AItem: string);
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.AddItem(const AItem: string);

  function ExtractName(const AURL: string): string;
  var
    I: Integer;
  begin
    Result := AURL;
    I := Result.LastIndexOf('/');
    Result := Result.Substring(I + 1);
    I := Result.IndexOf('.');
    Result := Result.Substring(0, I);
  end;

var
  LName: string;
begin
  LName := ExtractName(AItem);
  if ListBox1.Items.IndexOfName(LName) = -1 then
    ListBox1.Items.AddPair(LName,'http://www.stephaniequinn.com/' + AItem);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  MediaPlayer1.Stop;
  if TabControl1.TabIndex = 0 then
  begin
    if ListBox1.Selected <> nil then
    begin
      MediaPlayer1.FileName := ListBox1.Items.ValueFromIndex[ListBox1.ItemIndex];
      MediaPlayer1.Volume := 100;
      MediaPlayer1.Play;
    end;
  end;

  if TabControl1.TabIndex = 1 then
  begin
    if ListBox2.Selected <> nil then
    begin
      MediaPlayer1.FileName := ListBox2.Selected.Text;
      MediaPlayer1.Play;
    end;
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  s: string;
begin
  ListBox1.Items.Clear;
  s := IdHTTP1.Get(Edit1.Text);
  ScanText(s);
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  MediaPlayer1.Stop;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  ListBox2.Items.Clear;
  ListBox2.Items.Add('http://techslides.com/demos/samples/sample.avi');
  ListBox2.Items.Add('http://techslides.com/demos/samples/sample.mov');
  ListBox2.Items.Add('http://techslides.com/demos/samples/sample.mpg');
  ListBox2.Items.Add('http://techslides.com/demos/samples/sample.mp4');
  ListBox2.Items.Add('http://techslides.com/demos/samples/sample.wmv');
  ListBox2.Items.Add('http://techslides.com/demos/samples/sample.flv');
  ListBox2.Items.Add('http://techslides.com/demos/samples/sample.swf');
  ListBox2.Items.Add('http://techslides.com/demos/samples/sample.webm');
  ListBox2.Items.Add('http://techslides.com/demos/samples/sample.mkv');
end;

function TForm1.GetTagValue(var AText: string): string;
const
  TagName = '<a href=';
  TagEnd = '>';
var
  NextTagStartPosition: Integer;
  NextTagEndPosition: Integer;
begin
  Result := '';
  NextTagStartPosition := AText.IndexOf(TagName);
  if NextTagStartPosition > 0 then
  begin
    AText := AText.Substring(NextTagStartPosition + TagName.Length);
    NextTagEndPosition := AText.IndexOf(TagEnd);
    if NextTagEndPosition > 0 then
      Result := AText.Substring(1, NextTagEndPosition - 2);
  end;
  if Result.IsEmpty then
    AText := AText.Empty;
end;

function TForm1.IsItemCorrect(const AItem: string): Boolean;
const
  MP3Mask = '.mp3';
begin
  Result := AItem.Contains(MP3Mask);
end;

procedure TForm1.ScanText(const AText: string);
var
  S: string;
  Item: string;
begin
  S := AText;
  ProgressBar1.Max := AText.Length;
  ProgressBar1.Value := 0;
  while not S.IsEmpty do
  begin
    Item := GetTagValue(S);
    if IsItemCorrect(Item) then
      AddItem(Item);
    ProgressBar1.Value := ProgressBar1.Max - S.Length;
  end;
  ProgressBar1.Value := ProgressBar1.Max;
end;

end.
