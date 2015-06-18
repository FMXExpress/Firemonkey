unit UPrinting;

interface
uses UProgressThread, SysUtils, FMX.FlexCel.Core, FlexCel.Render;

type
  TPrintingThread = class(TProgressThread)
  private
    procedure ShowProgress(const sender: TObject; const e: TPrintPageEventArgs);
  protected
    procedure Execute; override;
  end;

implementation

{ TPrintingThread }

procedure TPrintingThread.Execute;
var
  doc: TFlexCelPrintDocument;
begin
  inherited;
  doc := TFlexCelPrintDocument.Create(Xls);
  try
    doc.AfterGeneratePage := ShowProgress;
    if AllSheets then
    begin
      doc.BeginPrint;
      try
        doc.PrintAllVisibleSheets(false);
      finally
        doc.EndPrint;
      end;
    end else
    begin
      doc.Print;
    end;
  finally
    FreeAndNil(doc);
  end;
end;

procedure TPrintingThread.ShowProgress(const sender: TObject;
  const e: TPrintPageEventArgs);
var
  Prog: TFlexCelPrintingProgress;
  Percent: Integer;
begin
  Prog := (Sender as TFlexCelPrintDocument).Progress;
  if (Prog.TotalPage = 0) then Percent := 100 else Percent := Round(Prog.Page * 100.0 / Prog.TotalPage);

  Synchronize(
  procedure
  begin
    ProgressFeedback(Percent, 'Page ' + IntToStr(Prog.Page) + ' of ' + IntToStr(Prog.TotalPage));
  end);
end;

end.
