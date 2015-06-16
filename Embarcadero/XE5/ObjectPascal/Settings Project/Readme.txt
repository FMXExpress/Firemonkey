{******************************************************************************}
{                                                                              }
{                         Delphi FireMonkey Platform                           }
{                                                                              }
{                            iOS Settings Demo                                 }
{                                                                              }
{                Copyright(c) 2013 Embarcadero Technologies, Inc.              }
{                                                                              }
{******************************************************************************}

This demo shows you how to use the style properties for TListBox to create a settings style listbox with
rounded corners. This is achieved by setting GroupingKind to gsGrouped and the StyleLookUp property to 
transparentlistboxstyle.

The properties for each ListBox item can be changed via the ItemData property in the Object Inspector.

A slide transition has been set via the ActionList component.

The SelectedValue member for each TComboBox component has been bound to the ItemData.Detail property via the 
LiveBindings Designer (View -> LiveBindings Designer).

Events can be set for each TSwitch component.


