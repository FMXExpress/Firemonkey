//---------------------------------------------------------------------------

#ifndef LifeEngineH
#define LifeEngineH
//---------------------------------------------------------------------------
#include <System.Types.hpp>
#include <System.SysUtils.hpp>
#include <System.Classes.hpp>
#include <System.Math.hpp>
#include <System.Diagnostics.hpp>
#include <System.Threading.hpp>
#include <memory>
#include <vector>

const TPoint NeighborDelta[] = {TPoint(-1, -1), TPoint(0, -1), TPoint(1, -1),
	TPoint(-1, 0), TPoint(1, 0), TPoint(-1, 1), TPoint(0, 1), TPoint(1, 1)};

enum TNeighbor {nbAboveLeft = 0, nbAbove, nbAboveRight, nbLeft, nbRight, nbBelowLeft, nbBelow, nbBelowRight};


class ELifeEngine : public Exception {
public:
	__fastcall ELifeEngine(const System::UnicodeString Msg): Exception(Msg){}
};

typedef System::DynamicArray<System::DynamicArray<System::Byte> > TLifeBoard;


typedef System::Set<int, 0, 8> TNeighborCounts;


class TLifeEngine : public TObject
{
private:
	class TLifeThread : public TThread
	{
	private:
		TLifeEngine * FLifeEngine;
		TLifeBoard FOriginalBoard;
		TLifeBoard FNewBoard;
		double FGensPerSecond;
		__int64 FElapsed;
		bool FParallel;
		TNeighborCounts FSurvival, FBirth;
		bool FUpdating;
		void __fastcall SetParallel(bool Value);
		int __fastcall WrapCoord(int Index, int ALow, int AHigh);
		bool __fastcall NeighborIsOccupied(TNeighbor ANeighbor, int Column, int Row);
		int __fastcall CountNeighbors(int Column, int Row);
		void __fastcall MyIteratorEvent(TObject* Sender, int AIndex);
	protected:
		void __fastcall UpdateGeneration(void);
		void __fastcall ProcessCells(TObject * Sender, int AIndex);
		void __fastcall Execute(void);
	public:
		__fastcall TLifeThread(TLifeEngine * ALifeEngine);
		__property bool Parallel = {read=FParallel, write=SetParallel};
	};
private:
	TLifeBoard FLifeBoard;
	TNeighborCounts FSurvival, FBirth;
	String FDescription;
	TLifeThread *FLifeThread;
	bool FParallel;
	TSize FBoardSize;
	double FGensPerSecond, FMaxGensPerSecond;
	__int64 FGraphCount;
	__int64 FGenerationCount;
	int FUpdateRate;
	TNotifyEvent FOnUpdate;
	bool __fastcall GetRunning(void);
	bool __fastcall GetParallel(void);
	void __fastcall SetParallel(bool Value);
	void __fastcall DoUpdate(void);
	void __fastcall SetUpdateRate(const int Value);
private:
	int __fastcall GetNumber(const String ALine, int & Index);
	bool __fastcall ProcessPattern(TStrings * APattern, TStrings *AData,
		TStrings * ADescription, TSize &ASize, TPoint &AOffset, int &LineNum,
		TNeighborCounts &ASurvival, TNeighborCounts &ABirth);
public:
	__fastcall TLifeEngine(const TSize ABoardSize);
	void __fastcall Clear(void);
	void __fastcall LoadPattern(const String AFileName);
	void __fastcall LoadPattern(TStream *AStream);
	void __fastcall Start(void);
	void __fastcall Stop(void);
	__property String Description = {read=FDescription};
	__property TLifeBoard LifeBoard = {read=FLifeBoard};
	__property bool Parallel = {read=GetParallel, write=SetParallel};
	__property bool Running = {read=GetRunning};
	__property double GensPerSecond = {read=FGensPerSecond};
	__property __int64 GenerationCount = {read=FGenerationCount};
	__property double MaxGensPerSecond = {read=FMaxGensPerSecond};
	__property int UpdateRate = {read=FUpdateRate, write=SetUpdateRate};
	__property TNotifyEvent OnUpdate = {read=FOnUpdate, write=FOnUpdate};
};

#endif
