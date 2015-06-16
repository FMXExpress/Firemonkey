unit URichTextView;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.TMSNativeNSBaseControl, FMX.TMSNativeNSTextView,
  FMX.TMSNativeNSRichTextView, FMX.TMSNativeNSButton, FMX.TMSNativeNSToolBar,
  FMX.TMSNativeNSView, FMX.TMSNativeNSRichTextViewToolBar,
  FMX.TMSNativeNSImageView, FMX.Edit, FMX.TMSNativeNSTextField,
  FMX.TMSNativeNSLabel, FMX.TMSNativeMacRichTextLib;

type
  TForm1121 = class(TForm)
    TMSFMXNativeNSRichTextView1: TTMSFMXNativeNSRichTextView;
    TMSFMXNativeNSToolbar1: TTMSFMXNativeNSToolbar;
    TMSFMXNativeNSRichTextViewToolBar1: TTMSFMXNativeNSRichTextViewToolBar;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1121: TForm1121;

implementation

{$R *.fmx}

procedure TForm1121.FormCreate(Sender: TObject);
var
  str: String;
  rt: String;
  val: TTMSFMXNativeMacRichTextLibParagraphStyle;
  l: Integer;
  sel: TTMSFMXNativeMacRichTextLibRange;
begin
  str := 'Hi !'#13#10#13#10;
  str := str + 'This is the new native iOS Rich Text View'#13#10;
  str := str + 'It has support for bold, italic, underline, strikethrough, subscript and superscript text'#13#10;
  str := str + 'Changing font size, font name, text color and background color'#13#10;
  str := str + 'Aligning text, and much more'#13#10;
  str := str + 'Isn''t that nice [0] , I [1] it !'#13#10;
  str := str + 'More information can be found on tmssoftware.com'#13#10;
  TMSFMXNativeNSRichTextView1.Text := str;
  str := TMSFMXNativeNSRichTextView1.Text;
  TMSFMXNativeNSRichTextView1.Font.Size := 16;


  rt := 'Hi !';
  TMSFMXNativeNSRichTextView1.SetFontSize(20, Pos(rt, str) - 1, length(rt));

  rt := 'Rich Text View';
  TMSFMXNativeNSRichTextView1.SetFont('Cochin', 16, Pos(rt, str) - 1, length(rt));

  rt := 'Rich';
  TMSFMXNativeNSRichTextView1.SetForegroundColor(TAlphaColorRec.Green, Pos(rt, str) - 1, length(rt));

  rt := 'Text';
  TMSFMXNativeNSRichTextView1.SetForegroundColor(TAlphaColorRec.Blue, Pos(rt, str) - 1, length(rt));

  rt := 'View';
  TMSFMXNativeNSRichTextView1.SetForegroundColor(TAlphaColorRec.Red, Pos(rt, str) - 1, length(rt));

  rt := 'bold';
  TMSFMXNativeNSRichTextView1.SetBold(True, Pos(rt, str) - 1, length(rt));

  rt := 'italic';
  TMSFMXNativeNSRichTextView1.SetItalic(True, Pos(rt, str) - 1, length(rt));

  rt := 'strikethrough';
  TMSFMXNativeNSRichTextView1.SetStrikethrough([usUnderlineStyleSingle], Pos(rt, str) - 1, length(rt));

  rt := 'subscript';
  TMSFMXNativeNSRichTextView1.SetSubscript(-2, Pos(rt, str) - 1, length(rt));
  TMSFMXNativeNSRichTextView1.SetFontSize(12, Pos(rt, str) - 1, length(rt));

  rt := 'superscript';
  TMSFMXNativeNSRichTextView1.SetSuperscript(3, Pos(rt, str) - 1, length(rt));
  TMSFMXNativeNSRichTextView1.SetFontSize(12, Pos(rt, str) - 1, length(rt));

  rt := 'underline';
  TMSFMXNativeNSRichTextView1.SetUnderline([usUnderlineStyleSingle], Pos(rt, str) - 1, length(rt));

  rt := 'underline';
  TMSFMXNativeNSRichTextView1.SetUnderlineColor(TAlphaColorRec.Lime, Pos(rt, str) - 1, length(rt));

  rt := 'text color';
  TMSFMXNativeNSRichTextView1.SetForegroundColor(TAlphaColorRec.Orange, Pos(rt, str) - 1, length(rt));

  rt := 'background color';
  TMSFMXNativeNSRichTextView1.SetBackgroundColor(TAlphaColorRec.Steelblue, Pos(rt, str) - 1, length(rt));
  TMSFMXNativeNSRichTextView1.SetForegroundColor(TAlphaColorRec.White, Pos(rt, str) - 1, length(rt));

  rt := 'font name';
  TMSFMXNativeNSRichTextView1.SetFont('Verdana', Pos(rt, str) - 1, length(rt));

  rt := 'font size';
  TMSFMXNativeNSRichTextView1.SetFontSize(28, Pos(rt, str) - 1, length(rt));

  FillChar(val, sizeof(TTMSFMXNativeMacRichTextLibParagraphStyle), 0);
  rt := 'Aligning text, and much more';
  val.Alignment := TAlignment.taRightJustify;
  TMSFMXNativeNSRichTextView1.SetParagraphStyle(val, Pos(rt, str) - 1, length(rt));

  rt := 'tmssoftware.com';
  TMSFMXNativeNSRichTextView1.SetURL('http://www.tmssoftware.com/site/tmsicl.asp', Pos(rt, str) - 1, length(rt));

  rt := '[0]';
  l := Pos(rt, str)-1;
  TMSFMXNativeNSRichTextView1.ReplaceText('', l, Length(rt));
  sel.location := l;
  sel.length := 0;
  TMSFMXNativeNSRichTextView1.Selection := sel;
  TMSFMXNativeNSRichTextView1.AddBitmapFromFile(ExtractFilePath(Paramstr(0)) + 'Cool.png');

  str := TMSFMXNativeNSRichTextView1.Text;

  rt := '[1]';
  l := Pos(rt, str)-1;
  TMSFMXNativeNSRichTextView1.ReplaceText('', l, Length(rt));
  sel.location := l;
  sel.length := 0;
  TMSFMXNativeNSRichTextView1.Selection := sel;
  TMSFMXNativeNSRichTextView1.AddBitmapFromFile(ExtractFilePath(Paramstr(0)) + 'Heart.png');

  TMSFMXNativeNSRichTextView1.TextView.resignFirstResponder;
end;

end.
