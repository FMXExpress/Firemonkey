//---------------------------------------------------------------------------

#pragma hdrstop

#include "LifeEngine.h"

//---------------------------------------------------------------------------
#pragma package(smart_init)

//---------------------------------------------------------------------------
__fastcall TLifeEngine::TLifeThread::TLifeThread(TLifeEngine * ALifeEngine)
	: TThread(true)
{
	FLifeEngine = ALifeEngine;
	FSurvival = ALifeEngine->FSurvival;
	FBirth = ALifeEngine->FBirth;
	FParallel = ALifeEngine->Parallel;
}
//---------------------------------------------------------------------------
void __fastcall TLifeEngine::TLifeThread::SetParallel(bool Value)
{
	FParallel = Value; // AtomicExchange don't implement C++Builder
}
//---------------------------------------------------------------------------
void __fastcall TLifeEngine::TLifeThread::UpdateGeneration(void)
{
	FLifeEngine->FLifeBoard = FNewBoard;
	FLifeEngine->FGensPerSecond = FGensPerSecond;
	FLifeEngine->FGraphCount = FLifeEngine->FGraphCount + FElapsed;
	if(FLifeEngine->FGraphCount > TStopwatch::Frequency *2) {
		FLifeEngine->FMaxGensPerSecond = 0.0;
		FLifeEngine->FGraphCount = 0;
	}
	FLifeEngine->FMaxGensPerSecond = Max(FLifeEngine->FMaxGensPerSecond, FLifeEngine->FGensPerSecond);
	FLifeEngine->DoUpdate();
	FUpdating = false;
}
//---------------------------------------------------------------------------
int __fastcall TLifeEngine::TLifeThread::WrapCoord(int Index, int ALow, int AHigh)
{
	int _return = Index;
	if(_return < ALow) {
		_return = AHigh;
	}
	else if(_return > AHigh) {
		_return = ALow;
	}
	return _return;
}
//---------------------------------------------------------------------------
bool __fastcall TLifeEngine::TLifeThread::NeighborIsOccupied(TNeighbor ANeighbor, int Column, int Row)
{
	TPoint Delta = NeighborDelta[ANeighbor];
	return FOriginalBoard[WrapCoord(Column + Delta.X, FOriginalBoard.Low, FOriginalBoard.High)]
		[WrapCoord(Row + Delta.Y, FOriginalBoard[0].Low, FOriginalBoard[0].High)] != 0;
}
//---------------------------------------------------------------------------
int __fastcall TLifeEngine::TLifeThread::CountNeighbors(int Column, int Row)
{
	int _return = 0;
	for(int i = 0; i <= nbBelowRight; i++) {
		if(NeighborIsOccupied((TNeighbor)i, Column, Row)) {
			_return++;
		}
	}
	return _return;
}
//---------------------------------------------------------------------------
void __fastcall TLifeEngine::TLifeThread::ProcessCells(TObject * Sender, int AIndex)
{
  for(int i = FOriginalBoard[AIndex].Low; i <= FOriginalBoard[AIndex].High; i++)
  {
	  int NeighborCount = CountNeighbors(AIndex, i);
	  if((FOriginalBoard[AIndex][i] != 0) && (FSurvival.Contains(NeighborCount))) {
		  FNewBoard[AIndex][i] = FOriginalBoard[AIndex][i]; // lives to next gen if occupied
	  }
	  else if((FOriginalBoard[AIndex][i] == 0) && (FBirth.Contains(NeighborCount))) {
		  FNewBoard[AIndex][i] = 1; // comes to life
	  }
	  else {
          FNewBoard[AIndex][i] = 0; // always dies
      }
  }
}
//---------------------------------------------------------------------------
void __fastcall TLifeEngine::TLifeThread::MyIteratorEvent(TObject* Sender, int AIndex)
{
    ProcessCells(Sender, AIndex);
}
//---------------------------------------------------------------------------
void __fastcall TLifeEngine::TLifeThread::Execute(void)
{
	TStopwatch Update, Timer;
	NameThreadForDebugging(UnicodeString("Life Thread"));
	Update = TStopwatch::StartNew();
	while(!Terminated) {
		Timer = TStopwatch::StartNew();
		FOriginalBoard = FLifeEngine->LifeBoard;
		FNewBoard.Length = FOriginalBoard.Length;
		for(int i = 0; i < FNewBoard.Length; i ++) {
			FNewBoard[i].Length = FOriginalBoard[0].Length;
		}
		if(Parallel) {
			TParallel::For(NULL, FOriginalBoard.Low, FOriginalBoard.High, MyIteratorEvent);
		}
		else {
			for(int i = 0; i < FOriginalBoard.High; i++) {
				ProcessCells(NULL, i);
			}
		}
		FGensPerSecond = (double)Timer.Frequency / Timer.ElapsedTicks;
		if(Update.ElapsedTicks >= (Update.Frequency / FLifeEngine->UpdateRate)) {
			FUpdating = true;
			Synchronize(UpdateGeneration);
			Update = TStopwatch::StartNew();
		}
		FLifeEngine->FGenerationCount++;
		FElapsed = Timer.ElapsedTicks;
    }
}
//---------------------------------------------------------------------------
bool __fastcall TLifeEngine::GetRunning(void)
{
	return (FLifeThread != NULL);
}
//---------------------------------------------------------------------------
bool __fastcall TLifeEngine::GetParallel(void)
{
	if(FLifeThread != NULL) {
		return FLifeThread->Parallel;
	}
	else {
        return FParallel;
    }
}
//---------------------------------------------------------------------------
void __fastcall TLifeEngine::SetParallel(bool Value)
{
	if(FLifeThread != NULL) {
		FLifeThread->Parallel = Value;
	}
	FParallel = Value;
}
//---------------------------------------------------------------------------
void __fastcall TLifeEngine::DoUpdate(void)
{
	if(FOnUpdate) {
		FOnUpdate(this);
	}
}
//---------------------------------------------------------------------------
void __fastcall TLifeEngine::SetUpdateRate(const int Value)
{
	FUpdateRate = Value; // AtomicExchange don't implement C++Builder
}
//---------------------------------------------------------------------------
__fastcall TLifeEngine::TLifeEngine(const TSize ABoardSize)
{
	FBoardSize = ABoardSize;
	FSurvival =  TNeighborCounts() << 2 << 3;
	FBirth =  TNeighborCounts() << 3;
	FUpdateRate = 60;
	Clear();
}
//---------------------------------------------------------------------------
void __fastcall TLifeEngine::Clear(void)
{
	if(Running) {
		throw ELifeEngine("Cannot clear life board while running");
	}
	TLifeBoard NewBoard;
	NewBoard.Length = FBoardSize.cx;
	for(int i = 0; i < NewBoard.Length; i++)  {
		NewBoard[i].Length = FBoardSize.cy;
		for(int j = 0; j < NewBoard[i].Length; j++){
			NewBoard[i][j] = 0;
		}
	}
	FLifeBoard = NewBoard;
	FGenerationCount = 0;
}
//---------------------------------------------------------------------------
void __fastcall TLifeEngine::LoadPattern(const String AFileName)
{
	std::auto_ptr<TStream> Stream(new TFileStream(AFileName, fmOpenRead | fmShareDenyWrite));
	Stream->Seek(0,0);
	LoadPattern(Stream.release());
}
//---------------------------------------------------------------------------
int __fastcall TLifeEngine::GetNumber(const String ALine, int & Index)
{
	while ((Index <= ALine.Length()) && ((ALine[Index] < '0') || (ALine[Index] > '9'))
		&& (ALine[Index] != '-') && (ALine[Index] != '+'))
	{
		Index++;
	}
	int Start = Index;
	while ((Index <= ALine.Length()) && (((ALine[Index] >= '0') && (ALine[Index] <= '9'))
		|| (ALine[Index] == '-') || (ALine[Index] == '+')))
	{
		Index++;
	}
	return StrToIntDef(ALine.SubString(Start - 1, Index - Start + 1), 0);
}
//---------------------------------------------------------------------------
bool __fastcall TLifeEngine::ProcessPattern(TStrings * APattern, TStrings *AData,
	TStrings * ADescription, TSize &ASize, TPoint &AOffset, int &LineNum,
	TNeighborCounts &ASurvival, TNeighborCounts &ABirth)
{

	ASize.cx = 0; ASize.cy = 0;
	AOffset.X = 0; AOffset.Y = 0;
	bool inPattern = false;
	String Line = "";
	int I = 0;
	while(LineNum < APattern->Count) {
		Line = APattern->Strings[LineNum];
		if((LineNum == 0) && (!SameText(Line, "#Life 1.05"))) {
			throw Exception("Invalid .lif or .life format.");
		}
		else if((Line.Length() > 0) && (Line[1] != '#')) {
			if(!inPattern) {
				AData->Clear();
			}
			inPattern = true;
			ASize.cy++;
			ASize.cx = Max(ASize.cx, Line.Length());
			AData->Add(Line);
		}
		else if((Line.Length() > 1) && (Line[1] == '#')) {
			if(inPattern) break;
			if(UpCase(Line[2]) == 'P') {
				I = 3;
				AOffset.X = GetNumber(Line, I);
				AOffset.Y = GetNumber(Line, I);
			}
			else if(UpCase(Line[2]) == 'N') {
				ASurvival =  TNeighborCounts() << 2 << 3;
				ABirth = TNeighborCounts() << 3;
			}
			else if(UpCase(Line[2]) == 'R') {
				ASurvival.Clear();
				ABirth.Clear();
				I = 3;
				while((Line[I] < '0') || (Line[I] > '8')) {
					I++;
                }
				while((Line[I] >= '0') && (Line[I] <= '8')) {
					ASurvival << StrToInt(Line[I]);
					I++;
				}
				if(Line[I] == '/') {
					while((Line[I] >= '0') && (Line[I] <= '8')) {
						ABirth << StrToInt(Line[I]);
						I++;
                    }
				}
			}
			else if((UpCase(Line[2]) == 'D') || (UpCase(Line[2]) == 'C')) {
					ADescription->Add(Line.SubString(3, MaxInt));
			}
		}
		LineNum++;
	}
	return (LineNum < APattern->Count);
}
//---------------------------------------------------------------------------
void __fastcall TLifeEngine::LoadPattern(TStream *AStream)
{
	std::auto_ptr<TStringList> Pattern(new TStringList());
	std::auto_ptr<TStringList> Data(new TStringList());
	std::auto_ptr<TStringList> Description(new TStringList());
	Pattern->LoadFromStream(AStream);
	Pattern->Add("#E");
	int CurLine = 0;
	TSize Size;
	TPoint Offset, Origin;
	String Line;
	while(ProcessPattern(Pattern.get(), Data.get(), Description.get(), Size, Offset, CurLine, FSurvival, FBirth)) {
		if((Size.cx > FLifeBoard.Length) || (Size.cy > FLifeBoard[0].Length)) {
			throw Exception(String().sprintf(L"Pattern too large for current board size.  Pattern Size (%d, %d)", Size.cx, Size.cy));
		}
		Origin = System::Classes::Point(FLifeBoard.Length/2 + Offset.X, FLifeBoard[0].Length/2 + Offset.Y);
		for(int y = 0; y < Data->Count; y++) {
			Line = Data->Strings[y];
			for(int x = 1; x < Line.Length()+1; x++) {
				if(Line[x] == '*') {
					FLifeBoard[Origin.X + (x-1)][Origin.Y + y] = 1;
				}
				else {
                    FLifeBoard[Origin.X + (x-1)][Origin.Y + y] = 0;
                }
			}
		}
		FDescription = Description->Text;
	}
	if(!Running) {
			DoUpdate();
	}
}
//---------------------------------------------------------------------------
void __fastcall TLifeEngine::Start(void)
{
	if(!Running) {
		FLifeThread = new TLifeThread(this);
		FLifeThread->Start();
	}
	else {
		throw ELifeEngine("Life Engine is already running");
	}
}
//---------------------------------------------------------------------------
void __fastcall TLifeEngine::Stop(void)
{
	if(Running) {
		FLifeThread->Terminate();
		FLifeThread->WaitFor();
		delete FLifeThread;
		FLifeThread = NULL;
	}
	else {
		throw ELifeEngine("Life Engine is not running");
	}
}
//---------------------------------------------------------------------------

