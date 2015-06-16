unit URichTextView;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Dialogs,
  FMX.TMSNativeUIView, FMX.TMSNativeUIRichTextViewToolBar, FMX.TMSNativeUICore,
  FMX.TMSNativeUIBaseControl, FMX.TMSNativeUIRichTextView, FMX.TMSNativeRichTextLib, FMX.TMSNativeUIButton,
  FMX.TMSNativeUITextView;

type
  TForm1119 = class(TForm)
    TMSFMXNativeUIRichTextView1: TTMSFMXNativeUIRichTextView;
    TMSFMXNativeUIRichTextViewToolBar1: TTMSFMXNativeUIRichTextViewToolBar;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1119: TForm1119;

implementation

{$R *.fmx}

procedure TForm1119.FormCreate(Sender: TObject);
var
  str: String;
  rt: String;
  val: TTMSFMXNativeRichTextLibParagraphStyle;
begin
  str := 'Hi !'#13#10#13#10;
  str := str + 'This is the new native iOS Rich Text View'#13#10;
  str := str + 'It has support for bold, italic, underline, strikethrough, subscript and superscript text'#13#10;
  str := str + 'Changing font size, font name, text color and background color'#13#10;
  str := str + 'Aligning text, and much more'#13#10;
  str := str + 'Isn''t that nice '+chr($e405)+', I '+chr($e022)+' it !'#13#10;
  str := str + 'More information can be found on tmssoftware.com'#13#10;
  TMSFMXNativeUIRichTextView1.Text := str;
  str := TMSFMXNativeUIRichTextView1.Text;

  TMSFMXNativeUIRichTextView1.Font.Size := 16;


  rt := 'Hi !';
  TMSFMXNativeUIRichTextView1.SetFontSize(20, Pos(rt, str) - 1, length(rt));

  rt := 'Rich Text View';
  TMSFMXNativeUIRichTextView1.SetFont('Cochin', 16, Pos(rt, str) - 1, length(rt));

  rt := 'Rich';
  TMSFMXNativeUIRichTextView1.SetForegroundColor(TAlphaColorRec.Green, Pos(rt, str) - 1, length(rt));

  rt := 'Text';
  TMSFMXNativeUIRichTextView1.SetForegroundColor(TAlphaColorRec.Blue, Pos(rt, str) - 1, length(rt));

  rt := 'View';
  TMSFMXNativeUIRichTextView1.SetForegroundColor(TAlphaColorRec.Red, Pos(rt, str) - 1, length(rt));

  rt := 'bold';
  TMSFMXNativeUIRichTextView1.SetBold(True, Pos(rt, str) - 1, length(rt));

  rt := 'italic';
  TMSFMXNativeUIRichTextView1.SetItalic(True, Pos(rt, str) - 1, length(rt));

  rt := 'strikethrough';
  TMSFMXNativeUIRichTextView1.SetStrikethrough([usUnderlineStyleSingle], Pos(rt, str) - 1, length(rt));

  rt := 'subscript';
  TMSFMXNativeUIRichTextView1.SetSubscript(-2, Pos(rt, str) - 1, length(rt));
  TMSFMXNativeUIRichTextView1.SetFontSize(12, Pos(rt, str) - 1, length(rt));

  rt := 'superscript';
  TMSFMXNativeUIRichTextView1.SetSuperscript(3, Pos(rt, str) - 1, length(rt));
  TMSFMXNativeUIRichTextView1.SetFontSize(12, Pos(rt, str) - 1, length(rt));

  rt := 'underline';
  TMSFMXNativeUIRichTextView1.SetUnderline([usUnderlineStyleSingle], Pos(rt, str) - 1, length(rt));

  rt := 'underline';
  TMSFMXNativeUIRichTextView1.SetUnderlineColor(TAlphaColorRec.Lime, Pos(rt, str) - 1, length(rt));

  rt := 'text color';
  TMSFMXNativeUIRichTextView1.SetForegroundColor(TAlphaColorRec.Orange, Pos(rt, str) - 1, length(rt));

  rt := 'background color';
  TMSFMXNativeUIRichTextView1.SetBackgroundColor(TAlphaColorRec.Steelblue, Pos(rt, str) - 1, length(rt));
  TMSFMXNativeUIRichTextView1.SetForegroundColor(TAlphaColorRec.White, Pos(rt, str) - 1, length(rt));

  rt := 'font name';
  TMSFMXNativeUIRichTextView1.SetFont('Verdana', Pos(rt, str) - 1, length(rt));

  rt := 'font size';
  TMSFMXNativeUIRichTextView1.SetFontSize(28, Pos(rt, str) - 1, length(rt));

  rt := 'Isn''t that nice '+chr($e405)+', I '+chr($e022)+' it !';
  TMSFMXNativeUIRichTextView1.SetFontSize(18, Pos(rt, str) - 1, length(rt));

  FillChar(val, sizeof(TTMSFMXNativeRichTextLibParagraphStyle), 0);
  rt := 'Aligning text, and much more';
  val.Alignment := taRight;
  TMSFMXNativeUIRichTextView1.SetParagraphStyle(val, Pos(rt, str) - 1, length(rt));

  rt := 'tmssoftware.com';
  TMSFMXNativeUIRichTextView1.SetURL('http://www.tmssoftware.com/site/tmsicl.asp', Pos(rt, str) - 1, length(rt));

end;

end.
