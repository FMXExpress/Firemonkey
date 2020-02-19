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

#include <fmx.h>
#pragma hdrstop

#include "Unit1.h"
#include "iostream"
#include "string"
//---------------------------------------------------------------------------
#pragma package(smart_init)
#pragma resource "*.fmx"
TForm1 *Form1;
//---------------------------------------------------------------------------
__fastcall TForm1::TForm1(TComponent* Owner)
	: TForm(Owner)
{
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button3Click(TObject *Sender)
{
    MediaPlayer1->Stop();
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button1Click(TObject *Sender)
{
	MediaPlayer1->Stop();
	if(TabControl1->TabIndex == 0){
		if(ListBox1->Selected != NULL){
			MediaPlayer1->FileName = ListBox1->Items->ValueFromIndex[ListBox1->ItemIndex];
			MediaPlayer1->Play();
		}
	}

	if(TabControl1->TabIndex == 1){
		if(ListBox2->Selected != NULL){
			MediaPlayer1->FileName = ListBox2->Selected->Text;
			MediaPlayer1->Play();
		}
	}
}
//---------------------------------------------------------------------------
void __fastcall TForm1::Button2Click(TObject *Sender)
{
	ListBox1->Items->Clear();
	String s = IdHTTP1->Get(Edit1->Text);
	ScanText(s);
}
//---------------------------------------------------------------------------
void TForm1::ScanText(String AText){
	String S = AText;
	String Item;
	ProgressBar1->Max = AText.Length();
	ProgressBar1->Value = 0;
	while (!S.IsEmpty()){
		Item = GetTagValue(S);
		if(IsItemCorrect(Item)){
			AddItem(Item);
		}
		ProgressBar1->Value = ProgressBar1->Max - S.Length();
	}
	ProgressBar1->Value = ProgressBar1->Max;
}

#define TagName "<a href="
#define TagEnd ">"
#define Empty ""

String TForm1::GetTagValue(String & AText){
	Integer NextTagStartPosition = AText.Pos(TagName);
	Integer	NextTagEndPosition;
	String LTagName = TagName;
	String LResult = Empty;
	if(NextTagStartPosition > 0){
		AText.Delete(1, NextTagStartPosition + LTagName.Length());
		NextTagEndPosition = AText.Pos(TagEnd);
		if(NextTagEndPosition > 0){
			LResult = AText.SubString(1, NextTagEndPosition - 2);
		}
	}
	if(LResult.IsEmpty())
		AText = Empty;
	return LResult;
}

#define MP3Mask ".mp3"

System::Boolean TForm1::IsItemCorrect(String AItem){
	return AItem.Pos(MP3Mask) > 0;
}

String ExtractName(String AURL){
	Integer I;
	while((I = AURL.Pos('/')) > 0){
		AURL.Delete(1, I);
	}
	while((I = AURL.Pos("%20")) > 0){
		AURL.Delete(I, 3);
	}
	I = AURL.Pos('.');
	return AURL.SubString(0, I);
}

#define BaseUrl "http:\/\/www.stephaniequinn.com\/"

void TForm1::AddItem(String AItem){
	String LName = ExtractName(AItem);
	if((!LName.IsEmpty())&&(ListBox1->Items->IndexOfName(LName) == -1))
		ListBox1->Items->AddPair(LName, BaseUrl + AItem);
}
void __fastcall TForm1::FormCreate(TObject *Sender)
{
  ListBox2->Items->Clear();
  ListBox2->Items->Add("http:\/\/techslides.com\/demos\/samples\/sample.avi");
  ListBox2->Items->Add("http:\/\/techslides.com\/demos\/samples\/sample.mov");
  ListBox2->Items->Add("http:\/\/techslides.com\/demos\/samples\/sample.mpg");
  ListBox2->Items->Add("http:\/\/techslides.com\/demos\/samples\/sample.mp4");
  ListBox2->Items->Add("http:\/\/techslides.com\/demos\/samples\/sample.wmv");
  ListBox2->Items->Add("http:\/\/techslides.com\/demos\/samples\/sample.flv");
  ListBox2->Items->Add("http:\/\/techslides.com\/demos\/samples\/sample.swf");
  ListBox2->Items->Add("http:\/\/techslides.com\/demos\/samples\/sample.webm");
  ListBox2->Items->Add("http:\/\/techslides.com\/demos\/samples\/sample.mkv");
}
//---------------------------------------------------------------------------

