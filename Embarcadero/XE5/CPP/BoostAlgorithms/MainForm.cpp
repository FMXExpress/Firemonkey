//---------------------------------------------------------------------------

#include <fmx.h>
#pragma hdrstop

#ifndef _WIN64
#error "This demo only supports Win64 target
#endif

#include "MainForm.h"
#include "System.SysUtils.hpp"
#include <vector>
#include <boost/algorithm/cxx11/all_of.hpp>
#include <boost/algorithm/cxx11/any_of.hpp>
#include <boost/algorithm/cxx11/none_of.hpp>
#include <boost/algorithm/cxx11/one_of.hpp>
#include <boost/algorithm/cxx11/is_sorted.hpp>
#include <boost/algorithm/cxx11/is_partitioned.hpp>
#include <boost/algorithm/cxx11/partition_point.hpp>
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
void __fastcall TForm1::AddToListButtonClick(TObject *Sender)
{
  ListBox1->Items->Add(NumberBox1->Text);
}
//---------------------------------------------------------------------------

bool isOdd ( int i ) { return i % 2 == 1; }
bool isEven ( int i) { return i % 2 == 0; }
bool lessThan10 ( int i ) { return i < 10; }
bool greaterThan10 ( int i ) { return i > 10; }

void __fastcall TForm1::TestButtonClick(TObject *Sender)
{
	Memo1->Lines->Clear();
	// to test all_of you need a list of at least one element
	if (ListBox1->Count > 0) {
		// copy listbox items to a vector
		std::vector<int> numbers;
		for (auto i=0;i<ListBox1->Count;i++) {
			numbers.push_back(StrToInt(ListBox1->Items->Strings[i]));
		}

		// all_of tests

		if (boost::algorithm::all_of(numbers, isOdd)) {
			Memo1->Lines->Add("All of the Numbers are Odd");
		}
		if (boost::algorithm::all_of(numbers, isEven)) {
			Memo1->Lines->Add("All of the Numbers are Even");
		}
		if (boost::algorithm::all_of(numbers.begin(), numbers.end(), isEven)) {
			Memo1->Lines->Add("All of the Numbers are Even");
		}
		if (boost::algorithm::all_of_equal(numbers, StrToInt(NumberBox2->Text))) {
			Memo1->Lines->Add("All of the Numbers = "+NumberBox2->Text);
		}
		if (boost::algorithm::all_of_equal(numbers.begin(), numbers.begin()+3, StrToInt(NumberBox2->Text))) {
			Memo1->Lines->Add("The first 3 Numbers = "+NumberBox2->Text);
		}
		if (boost::algorithm::all_of(numbers.begin(), numbers.begin()+3, isEven)) {
			Memo1->Lines->Add("The first 3 Numbers are Even");
		}

		// any_of tests

		if (boost::algorithm::any_of(numbers, isOdd)) {
			Memo1->Lines->Add("Any of the Numbers are Odd");
		}
		if (boost::algorithm::any_of(numbers, lessThan10)) {
			Memo1->Lines->Add("Any of the Numbers are less than 10");
		}

		// none_of tests

		if (boost::algorithm::none_of(numbers, isOdd)) {
			Memo1->Lines->Add("None of the Numbers are Odd");
		}
		if (boost::algorithm::none_of_equal(numbers.begin(), numbers.begin()+3, StrToInt(NumberBox2->Text))) {
			Memo1->Lines->Add("None of the first 3 Numbers = "+NumberBox2->Text);
		}

		// one_of (and only one) tests

		if (boost::algorithm::one_of(numbers, isOdd)) {
			Memo1->Lines->Add("One of the Numbers is Odd");
		}
		if (boost::algorithm::one_of_equal(numbers.begin(), numbers.begin()+3, StrToInt(NumberBox2->Text))) {
			Memo1->Lines->Add("One of the first 3 Numbers = "+NumberBox2->Text);
		}

		// is_sorted tests

		if (boost::algorithm::is_sorted(numbers)) {
			Memo1->Lines->Add("The list of numbers is sorted");
		}
		if (boost::algorithm::is_sorted(numbers.begin(),numbers.begin()+3)) {
			Memo1->Lines->Add("The first 3 numbers are sorted");
		}
		if (boost::algorithm::is_increasing(numbers)) {
			Memo1->Lines->Add("The list of numbers is increasing");
		}

		// is_partitioned tests

		if (boost::algorithm::is_partitioned(numbers, isOdd)) {
			Memo1->Lines->Add("The list of numbers is partitioned odd");
		}

		// partition_point tests

		Memo1->Lines->Add("The partition point for lessThan10 = "
		 + IntToStr(partition_point(numbers.begin(),numbers.end(), lessThan10) - numbers.begin())
		);


	}
	else {
		Memo1->Lines->Add("You need at least one item in the list!");
	}
}
//---------------------------------------------------------------------------
void __fastcall TForm1::ClearListButtonClick(TObject *Sender)
{
  ListBox1->Items->Clear();
}
//---------------------------------------------------------------------------
