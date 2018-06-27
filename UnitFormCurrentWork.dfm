object FormCurrentWork: TFormCurrentWork
  Left = 0
  Top = 0
  Caption = 'FormCurrentWork'
  ClientHeight = 630
  ClientWidth = 1124
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 377
    Top = 0
    Width = 5
    Height = 630
    ExplicitLeft = 407
    ExplicitTop = 61
    ExplicitHeight = 594
  end
  object VirtualStringTree1: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 377
    Height = 630
    Align = alLeft
    BorderStyle = bsNone
    DefaultNodeHeight = 25
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    Header.AutoSizeIndex = 0
    Header.Font.Charset = DEFAULT_CHARSET
    Header.Font.Color = clWindowText
    Header.Font.Height = -13
    Header.Font.Name = 'Tahoma'
    Header.Font.Style = []
    Header.Height = 30
    Header.Options = [hoColumnResize, hoDrag, hoShowImages, hoShowSortGlyphs, hoVisible]
    ParentFont = False
    TabOrder = 0
    TreeOptions.MiscOptions = [toAcceptOLEDrop, toCheckSupport, toFullRepaintOnResize, toInitOnSave, toToggleOnDblClick, toWheelPanning, toEditOnClick]
    OnBeforeCellPaint = VirtualStringTree1BeforeCellPaint
    OnChange = VirtualStringTree1Change
    OnChecked = VirtualStringTree1Checked
    OnGetText = VirtualStringTree1GetText
    OnPaintText = VirtualStringTree1PaintText
    OnGetImageIndex = VirtualStringTree1GetImageIndex
    Columns = <
      item
        Position = 1
        Width = 208
        WideText = #1054#1087#1077#1088#1072#1094#1080#1103
      end
      item
        Position = 0
        WideText = #8470
      end
      item
        Position = 2
        Width = 100
        WideText = #1042#1088#1077#1084#1103
      end>
  end
  object RichEdit1: TRichEdit
    Left = 382
    Top = 0
    Width = 742
    Height = 630
    Align = alClient
    BorderStyle = bsNone
    Font.Charset = RUSSIAN_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 1
    Zoom = 100
    OnContextPopup = RichEdit1ContextPopup
    ExplicitLeft = 8
    ExplicitTop = 8
    ExplicitWidth = 684
    ExplicitHeight = 511
  end
end
