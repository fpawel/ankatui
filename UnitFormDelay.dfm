object FormDelay: TFormDelay
  Left = 0
  Top = 0
  Caption = 'FormDelay'
  ClientHeight = 41
  ClientWidth = 888
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 18
  object PanelPlaceHolder: TPanel
    Left = 0
    Top = 0
    Width = 848
    Height = 41
    Align = alClient
    Alignment = taLeftJustify
    BevelOuter = bvNone
    Color = clHighlightText
    ParentBackground = False
    TabOrder = 0
    ExplicitLeft = 73
    ExplicitWidth = 815
    ExplicitHeight = 44
    object ProgressBar1: TProgressBar
      Left = 0
      Top = 23
      Width = 848
      Height = 18
      Align = alBottom
      TabOrder = 0
      ExplicitTop = 26
      ExplicitWidth = 815
    end
    object Panel1: TPanel
      Left = 0
      Top = 0
      Width = 848
      Height = 23
      Align = alClient
      Alignment = taLeftJustify
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitWidth = 815
      ExplicitHeight = 26
      object LabelCurrentTime: TLabel
        Left = 113
        Top = 0
        Width = 58
        Height = 23
        Align = alLeft
        Caption = '00:00:00'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        ExplicitLeft = 138
      end
      object LabelTotalTime: TLabel
        Left = 39
        Top = 0
        Width = 58
        Height = 23
        Align = alLeft
        Caption = '00:00:00'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        ExplicitLeft = 74
        ExplicitHeight = 18
      end
      object LabelWhat: TLabel
        Left = 0
        Top = 0
        Width = 23
        Height = 23
        Align = alLeft
        Caption = '?dd'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clNavy
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
      end
      object LabelProgress: TLabel
        Left = 187
        Top = 0
        Width = 31
        Height = 23
        Align = alLeft
        Caption = '50%'
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clTeal
        Font.Height = -15
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentFont = False
        ExplicitHeight = 18
      end
      object Panel14: TPanel
        Left = 23
        Top = 0
        Width = 16
        Height = 23
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitLeft = 0
        ExplicitTop = -2
      end
      object Panel2: TPanel
        Left = 97
        Top = 0
        Width = 16
        Height = 23
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 1
        ExplicitLeft = 80
      end
      object Panel4: TPanel
        Left = 218
        Top = 0
        Width = 16
        Height = 23
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 2
        ExplicitLeft = 238
        ExplicitTop = -6
      end
      object Panel5: TPanel
        Left = 171
        Top = 0
        Width = 16
        Height = 23
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 3
        ExplicitLeft = 203
        ExplicitTop = -2
      end
    end
  end
  object ToolBar2: TToolBar
    Left = 848
    Top = 0
    Width = 40
    Height = 41
    Align = alRight
    ButtonHeight = 40
    ButtonWidth = 40
    Caption = 'ToolBar1'
    EdgeInner = esNone
    EdgeOuter = esNone
    Images = Form1.ImageList4
    TabOrder = 1
    ExplicitHeight = 40
    object ToolButton1: TToolButton
      Left = 0
      Top = 0
      Hint = #1055#1088#1086#1087#1091#1089#1090#1080#1090#1100' '#1079#1072#1076#1077#1088#1078#1082#1091
      Caption = 'ToolButtonSettings'
      ImageIndex = 4
      ParentShowHint = False
      ShowHint = True
      OnClick = ToolButton1Click
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 328
    Top = 8
  end
end
