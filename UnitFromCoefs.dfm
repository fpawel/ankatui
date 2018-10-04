object FormCoefs: TFormCoefs
  Left = 0
  Top = 0
  Caption = 'FormCoefs'
  ClientHeight = 300
  ClientWidth = 635
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object StringGrid3: TStringGrid
    Left = 0
    Top = 0
    Width = 635
    Height = 300
    Align = alClient
    BorderStyle = bsNone
    ColCount = 3
    DefaultDrawing = False
    FixedColor = clBackground
    FixedCols = 0
    RowCount = 1
    FixedRows = 0
    GradientEndColor = clBlack
    Options = [goFixedVertLine, goFixedHorzLine, goVertLine, goHorzLine, goRangeSelect, goColSizing, goEditing]
    TabOrder = 0
    OnDblClick = StringGrid3DblClick
    OnDrawCell = StringGrid3DrawCell
    OnKeyPress = StringGrid3KeyPress
    OnSelectCell = StringGrid3SelectCell
    OnSetEditText = StringGrid3SetEditText
    ColWidths = (
      64
      64
      64)
    RowHeights = (
      24)
  end
  object CheckBox3: TCheckBox
    Left = 36
    Top = 88
    Width = 143
    Height = 34
    Caption = 'CheckBox1'
    TabOrder = 1
    OnClick = CheckBox3Click
  end
end
