program RichTextView;

uses
  System.StartUpCopy,
  FMX.Forms,
  URichTextView in 'URichTextView.pas' {Form1119};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1119, Form1119);
  Application.Run;
end.
