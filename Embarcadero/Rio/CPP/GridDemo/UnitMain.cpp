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
// ---------------------------------------------------------------------------

#include <fmx.h>
#include <limits>
#define NaN (std::numeric_limits<double>::quiet_NaN())
#define Infinity (std::numeric_limits<double>::infinity())
#pragma hdrstop

#include "UnitMain.h"
// ---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TMainForm *MainForm;

template <typename T> struct EnumValue {
	T Value;
	UnicodeString Text;
};

const EnumValue<TGridOption> GridOptionValues[] =
	{
		{TGridOption::AlternatingRowBackground, "AlternatingRowBackground"},
		{TGridOption::Editing, "Editing"},
		{TGridOption::AlwaysShowEditor, "AlwaysShowEditor"},
		{TGridOption::ColumnResize, "ColumnResize"},
		{TGridOption::ColumnMove, "ColumnMove"},
		{TGridOption::ColLines, "ColLines"},
		{TGridOption::RowLines, "RowLines"},
		{TGridOption::RowSelect, "RowSelect"},
		{TGridOption::AlwaysShowSelection, "AlwaysShowSelection"},
		{TGridOption::Tabs, "Tabs"},
		{TGridOption::Header, "Header"},
		{TGridOption::HeaderClick, "HeaderClick"},
		{TGridOption::CancelEditingByDefault, "CancelEditingByDefault"},
		{TGridOption::AutoDisplacement, "AutoDisplacement"}
	};

const EnumValue<TCellReturnAction> CellReturnActionValues[] =
	{
		{TCellReturnAction::None, "None"},
		{TCellReturnAction::GotoNextRow, "GotoNextRow"},
		{TCellReturnAction::GotoNextColumn, "GotoNextColumn"}
	};

class TGridTypeInfo : public TObject {
	TGridOption FOption;
	TCellReturnAction FCellReturnAction;
	TGridDrawState FState;

__published:
	__property TGridOption Option = {read = FOption};
	__property TCellReturnAction CellReturnAction = {read = FCellReturnAction};
	__property TGridDrawState State = {read = FState};
};

void __fastcall TMainForm::InitStringValue(const int ColumnIndex, int &RowIndex, const String Caption,
	const String Value) {
	StringGrid1->Cells[2][RowIndex] = Caption;
	StringGrid1->Cells[3][RowIndex] = "'" + Value + "'";
	StringGrid1->Cells[ColumnIndex][RowIndex] = Value;
	RowIndex++;
};

void __fastcall TMainForm::MemoAddLine(const String EventName, const String Text) {
	TVarRec LVarRec[2];
	LVarRec[0] = EventName;
	LVarRec[1] = Text;
	Memo1->Lines->Add(Format("%s(%s)", LVarRec, 1));
	Memo1->SelStart = Memo1->Text.Length() - 1;
	Memo1->SelLength = 0;
};

void __fastcall TMainForm::UpdateTreeView() {
	if (!FTreeUpdating) {
		FTreeUpdating = True;
		try {
			for (int i = 0; i < FItemGridOptions->Count; i++) {
				try {
					FItemGridOptions->Items[i]->Enabled = True;
					FItemGridOptions->Items[i]->IsChecked =
						StringGrid1->Options.Contains(GridOptionValues[i].Value);
				}
				catch (...) {
					FItemGridOptions->Items[i]->Enabled = False;
					FItemGridOptions->Items[i]->IsChecked = False;
				}
			};

			FItemGridOptions->IsChecked = (StringGrid1->Options != TGridModel::DefaultGridOptions);
			for (int i = 0; i < FItemCellReturn->Count; i++) {
				try {
					FItemCellReturn->Items[i]->Enabled = True;
					FItemCellReturn->Items[i]->IsChecked = CellReturnActionValues[i].Value == StringGrid1->Model->CellReturnAction;
				}
				catch (...) {
					FItemCellReturn->Items[i]->Enabled = False;
					FItemCellReturn->Items[i]->IsChecked = False;
				}
			}
			FItemCellReturn->IsChecked = (StringGrid1->Model->CellReturnAction != TCellReturnAction::None);
			FDefaultDrawing->IsChecked = StringGrid1->DefaultDrawing;
		}
		__finally {
			FTreeUpdating = False;
		}
	}
}

void __fastcall TMainForm::UpdateGridByTreeView() {
	if (!FTreeUpdating) {
		TGridOptions Options;
		Options.Clear();
		for (int i = 0; i < FItemGridOptions->Count; i++) {
			if (FItemGridOptions->Items[i]->IsChecked) {
				Options = Options << GridOptionValues[i].Value;
			}
		}

		FItemGridOptions->IsChecked = (Options != TGridModel::DefaultGridOptions);
		TCellReturnAction ReturnAction = TCellReturnAction::None;
		for (int i = 0; i < FItemCellReturn->Count; i++) {
			if (FItemCellReturn->Items[i]->IsChecked) {
				ReturnAction = CellReturnActionValues[i].Value;
			}
		}
		FItemCellReturn->IsChecked = ReturnAction != TCellReturnAction::None;
		StringGrid1->Options = Options;
		StringGrid1->Model->CellReturnAction = ReturnAction;
		StringGrid1->DefaultDrawing = FDefaultDrawing->IsChecked;
	}
	UpdateTreeView();
	PropagateOptions();
}

void __fastcall TMainForm::InternalPropagate(TGrid * const Grid) {
	Grid->Options = StringGrid1->Options;
	Grid->Model->CellReturnAction = StringGrid1->Model->CellReturnAction;
	Grid->ControlType = StringGrid1->ControlType;
	for (int i = 0; i < Min(Grid->ColumnCount, StringGrid1->ColumnCount); ++i) {
		Grid->Columns[i]->Visible = StringGrid1->Columns[i]->Visible;
		Grid->Columns[i]->Width = StringGrid1->Columns[i]->Width;
		Grid->Columns[i]->Enabled = StringGrid1->Columns[i]->Enabled;
		Grid->Columns[i]->ReadOnly = StringGrid1->Columns[i]->ReadOnly;
	}
	Grid->DefaultDrawing = StringGrid1->DefaultDrawing;
	Grid->OnHeaderClick = StringGrid1->OnHeaderClick;
	Grid->OnSelChanged = StringGrid1->OnSelChanged;
	Grid->OnSelectCell = StringGrid1->OnSelectCell;
	Grid->OnDrawColumnCell = StringGrid1->OnDrawColumnCell;
	Grid->OnDrawColumnBackground = StringGrid1->OnDrawColumnBackground;
	Grid->OnDrawColumnHeader = StringGrid1->OnDrawColumnHeader;
	Grid->OnColumnMoved = StringGrid1->OnColumnMoved;
	Grid->OnEditingDone = StringGrid1->OnEditingDone;
	Grid->OnResize = StringGrid1->OnResize;
	Grid->OnTap = StringGrid1->OnTap;
}

void __fastcall TMainForm::PropagateOptions() {
	InternalPropagate(Grid1);
	InternalPropagate(Grid2);
}

// ---------------------------------------------------------------------------
__fastcall TMainForm::TMainForm(TComponent* Owner) : TForm(Owner) {
}
// ---------------------------------------------------------------------------

void __fastcall TMainForm::actnControlTypeUpdate(TObject *Sender) {
	actnControlType->Checked = StringGrid1->ControlType == TControlType::Platform;
}
// ---------------------------------------------------------------------------

void __fastcall TMainForm::actnControlTypeExecute(TObject *Sender) {
	TControlType NewControlType;
	if (StringGrid1->ControlType != TControlType::Platform) {
		NewControlType = TControlType::Platform;
	}
	else {
		NewControlType = TControlType::Styled;
	};
	StringGrid1->ControlType = NewControlType;
	PropagateOptions();
}


// ---------------------------------------------------------------------------
void __fastcall TMainForm::FormCreate(TObject *Sender) {
	ImageList1->CacheSize = 20;
	VertScrollBox1->Visible = False;

#pragma region TreeView updating
	FItemGridOptions = new TTreeViewItem(this);
	FItemGridOptions->Text = "TCustomGrid->Options";
	TreeView1->AddObject(FItemGridOptions);
	for (size_t i = 0; i < sizeof(GridOptionValues)/sizeof(GridOptionValues[0]); i++) {
		TTreeViewItem* Item = new TTreeViewItem(this);
		Item->Text = GridOptionValues[i].Text;
		FItemGridOptions->AddObject(Item);
	}
	FItemGridOptions->Expand();
	FItemCellReturn = new TTreeViewItem(this);
	FItemCellReturn->Text = "TGridModel->CellReturnAction";
	TreeView1->AddObject(FItemCellReturn);
	for (size_t i = 0; i < sizeof(CellReturnActionValues)/sizeof(CellReturnActionValues[0]); i++) {
		TTreeViewItem* Item = new TTreeViewItem(this);
		Item->Text = CellReturnActionValues[i].Text;
		FItemCellReturn->AddObject(Item);
	};

	FItemCellReturn->Expand();
	FDefaultDrawing = new TTreeViewItem(this);
	FDefaultDrawing->Text = "TCustomGrid->DefaultDrawing";
	TreeView1->AddObject(FDefaultDrawing);
	UpdateTreeView();
#pragma end_region
#pragma region Grid updating
	Grid1->RowCount = StringGrid1->RowCount;
	for (int i = 0; i < StringGrid1->ColumnCount; i++) {
		Grid1->AddObject(StringGrid1->Columns[i]->Clone(this));
		if (TProgressColumn *pcol = dynamic_cast<TProgressColumn*>(Grid1->Columns[Grid1->ColumnCount - 1])) {
			pcol->Max = Grid1->RowCount;
		}
	};
	FDMemTable1->Open();
	for (int i = 0; i < Min(Grid1->ColumnCount, Grid2->ColumnCount); i++) {
		Grid2->Columns[i]->Header = StringGrid1->Columns[i]->Header;
		Grid2->Columns[i]->Width = StringGrid1->Columns[i]->Width;
		if (TProgressColumn *pcol = dynamic_cast<TProgressColumn*>(Grid2->Columns[i])) {
			pcol->Max = Grid2->RowCount;
		}
	};
	TImageColumn* Column = new TImageColumn(this);
	Column->Header = Column->ClassName();
	Grid1->AddObject(Column);
	FBitmaps.set_length(Grid1->RowCount);
	for (int i = 0; i < FBitmaps.Length; i++) {
		FBitmaps[i] = NULL;
	}
	FStrings.set_length(Grid1->RowCount);
	FPopups.set_length(Grid1->RowCount);
	FDates.set_length(Grid1->RowCount);
	FTimes.set_length(Grid1->RowCount);
	for (int i = 0; i < Grid1->RowCount; i++) {
		FDates[i] = NaN;
		FTimes[i] = NaN;
	};
#pragma end_region
#pragma region String grid updating
	for (int i = 0; i < StringGrid1->RowCount; i++) {
		StringGrid1->Cells[0][i] = FloatToStr(i + 1 + (Random() * 0.1));
		StringGrid1->Cells[1][i] = IntToStr((i) % (ImageList1->Count));
		StringGrid1->Cells[2][i] = "Row # " + IntToStr(i);
	};
	int i = 1;
	InitStringValue(4, i, "Progress", "");
	InitStringValue(4, i, "Progress", "0");
	InitStringValue(4, i, "Progress Integer", "100");
	InitStringValue(4, i, "Progress Float", FloatToStr(333.33));
	InitStringValue(4, i, "Progress 50%", "500");
	InitStringValue(4, i, "Progress 100%", "1000");
	InitStringValue(4, i, "Progress more than 100%", "10000");
	InitStringValue(4, i, "Progress less than 0%", "-1");
	InitStringValue(4, i, "Progress error value", "Error");

	InitStringValue(5, i, "Check column", "");
	InitStringValue(5, i, "Check column", "t");
	InitStringValue(5, i, "Check column", "true");
	InitStringValue(5, i, "Check column", "False");
	InitStringValue(5, i, "Check column", "y");
	InitStringValue(5, i, "Check column", "N");
	InitStringValue(5, i, "Check column", "F");
	InitStringValue(5, i, "Check column", "0");
	InitStringValue(5, i, "Check column", "1");
	InitStringValue(5, i, "Check column", "-1");

	InitStringValue(6, i, "Date and Time",
		FormatDateTime(DateColumn1->Format + " " + System::Sysutils::FormatSettings.ShortTimeFormat,
		EncodeDate(1961, 4, 12) + EncodeTime(9, 7, 5, 333)));
	InitStringValue(6, i, "Date", DateToStr(EncodeDate(1957, 10, 4)));
	InitStringValue(6, i, "Long Date Format", FormatDateTime(System::Sysutils::FormatSettings.LongDateFormat,
		EncodeDate(1988, 11, 15)));
	InitStringValue(6, i, "Error Date", "Error");

	InitStringValue(7, i, "Date and Time", DateTimeToStr(EncodeDate(1961, 4, 12) + EncodeTime(9, 7, 5, 333)));
	InitStringValue(7, i, "Time", TimeToStr(EncodeTime(12, 45, 33, 777)));
	InitStringValue(7, i, "Long Time Format", FormatDateTime(System::Sysutils::FormatSettings.LongTimeFormat,
		EncodeTime(18, 15, 22, 555)));
	InitStringValue(7, i, "Error Time", "Error");

	InitStringValue(8, i, "Popup by Index", "1");
	InitStringValue(8, i, "Popup by Text", "interesting text");
	InitStringValue(8, i, "Popup by Text", "text");
#pragma end_region
	if (TCurrencyColumn* c = dynamic_cast<TCurrencyColumn*>(Grid2->Columns[0])) {
		c->DecimalDigits = 0;
	}
	FDMemTable1->DisableControls();
	for (int i = 0; i <= 200; i++) {
		FDMemTable1->Append();
		FDMemTable1Index->AsInteger = i + 1;
		FDMemTable1ImageIndex->AsInteger = i % ImageList1->Count;
		FDMemTable1String->AsString = "Row # " + IntToStr(i);
	};
	FDMemTable1->First();
	FDMemTable1Index->ReadOnly = True;
	FDMemTable1->EnableControls();
	PropagateOptions();
}
// ---------------------------------------------------------------------------

void __fastcall TMainForm::FormDestroy(TObject* Sender) {
	for (int i = 0; i < FBitmaps.Length; i++) {
		FBitmaps[i]->Free();
	}
}

// ---------------------------------------------------------------------------
void __fastcall TMainForm::actnVisibleColumnExecute(TObject* Sender) {
	StringGrid1->Columns[1]->Visible = !StringGrid1->Columns[1]->Visible;
	PropagateOptions();
}
// ---------------------------------------------------------------------------

void __fastcall TMainForm::actnVisibleColumnUpdate(TObject* Sender) {
	actnVisibleColumn->Checked = StringGrid1->Columns[1]->Visible;
}
// ---------------------------------------------------------------------------

void __fastcall TMainForm::actnWidthColumnExecute(TObject* Sender) {
	if (StringGrid1->Columns[1]->Width == 0.0) {
		StringGrid1->Columns[1]->Width = 32.0;
	}
	else {
		StringGrid1->Columns[1]->Width = 0.0;
	};
	PropagateOptions();
}
// ---------------------------------------------------------------------------

void __fastcall TMainForm::actnWidthColumnUpdate(TObject* Sender) {
	actnWidthColumn->Checked = StringGrid1->Columns[1]->Width == 0;
}

// ---------------------------------------------------------------------------
void __fastcall TMainForm::actnEnableColumnExecute(TObject * Sender) {
	StringGrid1->Columns[2]->Enabled = !StringGrid1->Columns[2]->Enabled;
	PropagateOptions();
}
// ---------------------------------------------------------------------------

void __fastcall TMainForm::actnEnableColumnUpdate(TObject* Sender) {
	actnEnableColumn->Checked = StringGrid1->Columns[2]->Enabled;
}
// ---------------------------------------------------------------------------

void __fastcall TMainForm::actnReadOnlyColumnExecute(TObject * Sender) {
	StringGrid1->Columns[2]->ReadOnly = !StringGrid1->Columns[2]->ReadOnly;
	PropagateOptions();
}
// ---------------------------------------------------------------------------

void __fastcall TMainForm::actnReadOnlyColumnUpdate(TObject* Sender) {
	actnReadOnlyColumn->Checked = StringGrid1->Columns[2]->ReadOnly;
}
// ---------------------------------------------------------------------------

void __fastcall TMainForm::actnOptionsExecute(TObject* Sender) {
	if (!VertScrollBox1->Visible) {
		Splitter1->Position->X = Width - VertScrollBox1->Width - Splitter1->Width;
		UpdateTreeView();
	};
	VertScrollBox1->Visible = !VertScrollBox1->Visible;
	Splitter1->Visible = VertScrollBox1->Visible;
}
// ---------------------------------------------------------------------------

void __fastcall TMainForm::actnOptionsUpdate(TObject* Sender) {
	actnOptions->Checked = VertScrollBox1->Visible;
}
// ---------------------------------------------------------------------------

void __fastcall TMainForm::Grid1GetValue(TObject* Sender, const int ACol, const int ARow, TValue& Value) {
	String ColumnClass = TColumnClass(Grid1->Columns[ACol]->ClassType())->ClassName();
	if (ColumnClass == TImageColumn::ClassName()) {
		Value = FBitmaps[ARow];
	}
	else if (ColumnClass == TColumn::ClassName()) {
		if (FBitmaps[ARow] == NULL) {
			Value = TValue::From<String>("NULL");
		}
		else if (FBitmaps[ARow]->IsEmpty()) {
			Value = SEmpty;
		}
		else {
			TVarRec LVarRec[2];
			LVarRec[0] = FBitmaps[ARow]->Width;
			LVarRec[1] = FBitmaps[ARow]->Height;
			Value = Format("Width: %d; Height: %d", LVarRec, 1);
		};
	}
	else if (ColumnClass == TCheckColumn::ClassName()) {
		if (FBitmaps[ARow] == NULL) {
			Value = TValue::Empty;
		}
		else if (FBitmaps[ARow]->IsEmpty()) {
			Value = False;
		}
		else {
			Value = True;
		};
	}
	else if (ColumnClass == TCurrencyColumn::ClassName()) {
		Value = ARow + 1 + (Random() * 0.1);
	}
	else if (ColumnClass == TGlyphColumn::ClassName()) {
		Value = ARow % ImageList1->Count;
	}
	else if (ColumnClass == TStringColumn::ClassName()) {
		Value = FStrings[ARow];
	}
	else if (ColumnClass == TPopupColumn::ClassName()) {
		Value = FPopups[ARow];
	}
	else if (ColumnClass == TProgressColumn::ClassName()) {
		Value = ARow;
	}
	else if (ColumnClass == TDateColumn::ClassName()) {
		if (::System::Math::IsNan((double)FDates[ARow])) {
			Value = TValue::Empty;
		}
		else {
			Value = TValue::From<TDateTime>(FDates[ARow]);
		}
	}
	else if (ColumnClass == TTimeColumn::ClassName()) {
		if (::System::Math::IsNan((double)FTimes[ARow])) {
			Value = TValue::Empty;
		}
		else {
			Value = TValue::From<TDateTime>(FTimes[ARow]);
		}
	}
}

// ---------------------------------------------------------------------------
void __fastcall TMainForm::Grid1SetValue(TObject * Sender, const int ACol, const int ARow, const TValue & Value) {
	String ColumnClass = TColumnClass(Grid1->Columns[ACol]->ClassType())->ClassName();
	TValue LValue = Value;
	if (ColumnClass == TImageColumn::ClassName()) {
		if ((!LValue.IsEmpty) && (LValue.IsObject()) && (LValue.AsObject()->ClassName() == TBitmap::ClassName())) {
			if (FBitmaps[ARow] == NULL) {
				FBitmaps[ARow] = new TBitmap;
			}
			FBitmaps[ARow]->Assign((TBitmap*)(LValue.AsObject()));
		}
	}
	else if (ColumnClass == TStringColumn::ClassName()) {
		FStrings[ARow] = LValue.ToString();
	}
	else if (ColumnClass == TPopupColumn::ClassName()) {
		FPopups[ARow] = LValue.ToString();
	}
	else if (ColumnClass == TDateColumn::ClassName()) {
		if (LValue.IsEmpty || !LValue.TryAsType<TDateTime>(FDates[ARow])) {
			FDates[ARow] = NaN;
		};
	}
	else if (ColumnClass == TTimeColumn::ClassName()) {
		if (LValue.IsEmpty || !LValue.TryAsType<TDateTime>(FTimes[ARow])) {
			FTimes[ARow] = NaN;
		};
	};
}

// ---------------------------------------------------------------------------
void __fastcall TMainForm::TreeView1ChangeCheck(TObject * Sender) {
	if (!FTreeUpdating) {
		FTreeUpdating = True;
		try {
			for (int i = 0; i < FItemCellReturn->Count; i++) {
				if (FItemCellReturn->Items[i] != Sender) {
					FItemCellReturn->Items[i]->IsChecked = False;
				};
			};
		}
		__finally {
			FTreeUpdating = False;
		};
	};
	UpdateGridByTreeView();
}

// ---------------------------------------------------------------------------
void __fastcall TMainForm::VertScrollBox1Resize(TObject* Sender) {
	if (VertScrollBox1->Width <= Splitter1->MinSize) {
		VertScrollBox1->Visible = False;
		Splitter1->Visible = False;
	}
}
// ---------------------------------------------------------------------------

void __fastcall TMainForm::DrawColumnBackgroundProc(TObject * Sender, TCanvas* const Canvas, TColumn* const Column,
	const TRectF& Bounds, const int Row, const TValue& Value, const TGridDrawStates State) {
	if (TCustomGrid* g = dynamic_cast<TCustomGrid*>(Sender)) {
		if (TStringColumn* c = dynamic_cast<TStringColumn*>(Column)) {
			TBrushKind OldKind = Canvas->Fill->Kind;
			TAlphaColor OldColor = Canvas->Fill->Color;
			TRectF R(Bounds);
			R.Inflate(-1, -1);
			if (State.Contains(TGridDrawState::Focused)) {
				Canvas->Fill->Color = TAlphaColorRec::Yellow;
			}
			else {
				Canvas->Fill->Color = TAlphaColorRec::Antiquewhite;
			}
			Canvas->Fill->Kind = TBrushKind::Solid;
			Canvas->FillRect(R, 0, 0, AllCorners, 1);
			Canvas->Fill->Color = OldColor;
			Canvas->Fill->Kind = OldKind;
		}
	}
}
// ---------------------------------------------------------------------------

void __fastcall TMainForm::DrawColumnCellProc(TObject* Sender, TCanvas* const Canvas, TColumn* const Column,
	const TRectF& Bounds, const int Row, const TValue& Value, const TGridDrawStates State) {
	if (TCustomGrid* g = dynamic_cast<TCustomGrid*>(Sender)) {
		if (TStringColumn* c = dynamic_cast<TStringColumn*>(Column)) {
			TBrushKind OldKind = Canvas->Stroke->Kind;
			Canvas->Stroke->Kind = TBrushKind::Solid;
			TRectF R(Bounds);
			R.Inflate(1.0 / 2 * Canvas->Scale, 1.0 / 2 * Canvas->Scale);
			Canvas->Stroke->Color = TAlphaColorRec::Aqua;
			Canvas->DrawRect(R, 0, 0, AllCorners, 1);
			Canvas->Stroke->Kind = OldKind;
			OldKind = Canvas->Fill->Kind;

			for (int i = int(TGridDrawState::Selected); i <= int(TGridDrawState::RowSelected); i++) {
				TGridDrawState S = TGridDrawState(i);
				if (State.Contains(S)) {
					TPointF P(Bounds.Left + ((int)S) * 3, Bounds.Top - 1);
					R.init(P.x, P.y, P.x + 3.0, P.Y + 3.0);
					switch (S) {
					case TGridDrawState::Selected: {
							Canvas->Fill->Color = TAlphaColorRec::Red;
						};
					case TGridDrawState::Focused: {
							Canvas->Fill->Color = TAlphaColorRec::Green;
						};
					case TGridDrawState::RowSelected: {
							Canvas->Fill->Color = TAlphaColorRec::Blue;
						};
					}
					Canvas->Fill->Kind = TBrushKind::Solid;
					Canvas->FillRect(R, 0, 0, AllCorners, 1);
				}
				Canvas->Fill->Kind = OldKind;
			};
		};
	}
}

// ---------------------------------------------------------------------------
void __fastcall TMainForm::DrawColumnHeaderProc(TObject* Sender, TCanvas* const Canvas, TColumn* const Column,
	const TRectF& Bounds) {
	if (TCustomGrid* g = dynamic_cast<TCustomGrid*>(Sender)) {
		if (TStringColumn* c = dynamic_cast<TStringColumn*>(Column)) {
			TBrushKind OldKind = Canvas->Stroke->Kind;
			TAlphaColor OldColor = Canvas->Stroke->Color;
			Canvas->Stroke->Kind = TBrushKind::Solid;
			TRectF R = Bounds;
			R.Inflate(-1.0 / 2 * Canvas->Scale, -1.0 / 2 * Canvas->Scale);
			Canvas->Stroke->Color = TAlphaColorRec::Lawngreen;
			Canvas->DrawRect(R, 0, 0, AllCorners, 1);
			Canvas->Stroke->Kind = OldKind;
			Canvas->Stroke->Color = OldColor;
		};
	}
}

// ---------------------------------------------------------------------------
void __fastcall TMainForm::EditingDoneProc(TObject* Sender, const int ACol, const int ARow) {
	String S;
	if (TControl* g = dynamic_cast<TControl*>(Sender)) {
		S = g->Name;
	}
	TVarRec LVarRec[3];
	LVarRec[0] = S;
	LVarRec[1] = ACol;
	LVarRec[2] = ARow;
	S = Format("[%s] %d; %d", LVarRec, 2);
	MemoAddLine("OnEditingDone", S);
}

// ---------------------------------------------------------------------------
void __fastcall TMainForm::HeaderClickProc(TColumn * Column) {
	String S;
	if (Column != NULL) {
		S = Column->Header;
		if (Column->PresentedControl != NULL) {
			S = "[" + Column->PresentedControl->Name + "] " + S;
		}
	}
	else {
		S = "nil";
	}
	MemoAddLine("OnHeaderClick", S);
}

// ---------------------------------------------------------------------------
void __fastcall TMainForm::ResizeProc(TObject * Sender) {
	String S;
	if (TControl* g = dynamic_cast<TControl*>(Sender)) {
		S = g->Name;
	}
	MemoAddLine("OnResize", S);
}

// ---------------------------------------------------------------------------
void __fastcall TMainForm::SelChangedProc(TObject * Sender) {
	String S;
	if (TControl* g = dynamic_cast<TControl*>(Sender)) {
		S = g->Name;
	}
	if (TCustomGrid *grid = dynamic_cast<TCustomGrid*>(Sender)) {
		TVarRec LVarRec[3];
		LVarRec[0] = S;
		LVarRec[1] = grid->Col;
		LVarRec[2] = grid->Row;
		S = Format("[%s] %d; %d", LVarRec, 2);
	};
	MemoAddLine("OnSelChanged", S);
}

// ---------------------------------------------------------------------------
void __fastcall TMainForm::ColumnMovedProc(TColumn * Column, int FromIndex, int ToIndex) {
	String S;
	if (Column != NULL) {
		S = Column->Header;
		if (Column->PresentedControl != NULL) {
			S = "[" + Column->PresentedControl->Name + "] " + S;
		}
		else {
			S = "nil";
		}
		TVarRec LVarRec[3];
		LVarRec[0] = S;
		LVarRec[1] = FromIndex;
		LVarRec[2] = ToIndex;
		S = Format("%s From: %d to %d", LVarRec, 2);
	}
	MemoAddLine("OnColumnMoved", S);
}

// ---------------------------------------------------------------------------
void __fastcall TMainForm::SelectCellProc(TObject * Sender, const int ACol, const int ARow, bool &CanSelect) {
	String S;
	if (TControl *g = dynamic_cast<TControl*>(Sender)) {
		S = g->Name;
	}
	TVarRec LVarRec[3];
	LVarRec[0] = S;
	LVarRec[1] = ACol;
	LVarRec[2] = ARow;
	S = Format("[%s] %d; %d", LVarRec, 2);

	if (ACol >= 0) {
		if (TCustomGrid* grid = dynamic_cast<TCustomGrid*>(Sender)) {
			if (TGlyphColumn* col = dynamic_cast<TGlyphColumn*>(grid->Columns[ACol])) {
				S += " CanSelect = False";
				CanSelect = False;
			};
		}
	}
	MemoAddLine("OnSelectCell", S);
}

// ---------------------------------------------------------------------------
void __fastcall TMainForm::TapProc(TObject * Sender, const TPointF & Point) {
	String S;
	if (TControl* g = dynamic_cast<TControl*>(Sender)) {
		S = ((TControl*)(Sender))->Name;
	}
	if (TCustomGrid* grid = dynamic_cast<TCustomGrid*>(Sender)) {
		TVarRec LVarRec[3];
		LVarRec[0] = S;
		LVarRec[1] = int(Point.X);
		LVarRec[2] = int(Point.Y);
		S = Format("[%s] %d; %d", LVarRec, 2);
		TPointF LocalPoint = ((TCustomGrid*)(Sender))->AbsoluteToLocal(Point);
		int Col, Row;
		if (grid->CellByPoint(LocalPoint.X, LocalPoint.Y, Col, Row)) {
			TVarRec LVarRec[2];
			LVarRec[0] = Col;
			LVarRec[1] = Row;
			S = S + Format(" Col: %d; Row: %d", LVarRec, 1);
		}
		else {
			S = S + " Col: ?; Row: ?";
		}
	}
	MemoAddLine("OnTap", S);
}
// ---------------------------------------------------------------------------
