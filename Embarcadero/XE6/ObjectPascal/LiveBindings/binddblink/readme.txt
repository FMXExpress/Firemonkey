Drop a TClientDataSet
Right click, Load from MyBase Table...
Choose FishFacts_With_BoolField.xml (from sample directory)

Drop a TDataSource
  Set DataSet property to ClientDataSet1

Drop a TBindSourceDB
  Set DataSource property to DataSource1

Drop a TBindNavigator
  Set BindScope to BindSourceDB1

Drop a TEdit
  In object inspector, click on "Link to DB Field..."
  Choose Category
  
Drop a TMemo
  In object inspector, click on "Link to DB Field..."
  Choose Notes
  Set WordWrap property to True

Drop a TCheckBox
  In object inspector, click on "Link to DB Field..."
  Choose Endangered
  Set Text property to "Endangered"

Drop a TImageControl
  In object inspector, click on "Link to DB Field..."
  Choose Graphic

Drop a TListBox
  Edit "Items" property, add "True" and "False" lines
  In object inspector, click on "Link to DB Field..."
  Choose Endangered

Drop a TComboBox
  In object inspector, click on "Link to DB Field..."
  Choose Endangered
  In object inspector, click on "Items Editor...
  Add an item, set text to "True"
  Add an item, Set text to "False"

Drop a TComboEdit
  Edit "Items" property, add "True" and "False" lines
  In object inspector, click on "Link to DB Field..."
  Choose Endangered

Drop a TStringGrid
  In object inspector, click on "Link to DB DataSource..."
  Choose BindSourceDB1
  In object inspector (for StringGrid1), choose "Columns Editor..." command
  Add all columns
  Remove unwanted columns, modify column widths in object inspector

Add Labels

Run application
  