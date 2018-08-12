object FormDelay: TFormDelay
  Left = 0
  Top = 0
  Caption = 'FormDelay'
  ClientHeight = 44
  ClientWidth = 647
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
    Width = 647
    Height = 44
    Align = alClient
    Alignment = taLeftJustify
    BevelOuter = bvNone
    Color = clHighlightText
    ParentBackground = False
    TabOrder = 0
    ExplicitLeft = -153
    ExplicitWidth = 788
    ExplicitHeight = 59
    object ProgressBar1: TProgressBar
      Left = 0
      Top = 26
      Width = 647
      Height = 18
      Align = alBottom
      TabOrder = 0
      ExplicitTop = 32
      ExplicitWidth = 635
    end
    object Panel1: TPanel
      Left = 0
      Top = 0
      Width = 647
      Height = 26
      Align = alClient
      Alignment = taLeftJustify
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitTop = 8
      ExplicitWidth = 635
      ExplicitHeight = 32
      object Panel2: TPanel
        Left = 170
        Top = 0
        Width = 308
        Height = 26
        Align = alClient
        Alignment = taLeftJustify
        BevelOuter = bvNone
        TabOrder = 0
        ExplicitLeft = 296
        ExplicitTop = 2
        ExplicitWidth = 217
        ExplicitHeight = 24
      end
      object Panel3: TPanel
        Left = 0
        Top = 0
        Width = 73
        Height = 26
        Align = alLeft
        BevelOuter = bvNone
        Caption = '00:00:00'
        TabOrder = 1
      end
      object Button1: TButton
        Left = 478
        Top = 0
        Width = 169
        Height = 26
        Align = alRight
        Caption = #1055#1088#1086#1087#1091#1089#1090#1080#1090#1100' '#1079#1072#1076#1077#1088#1078#1082#1091
        TabOrder = 2
        OnClick = Button1Click
      end
      object Panel4: TPanel
        Left = 73
        Top = 0
        Width = 24
        Height = 26
        Align = alLeft
        BevelOuter = bvNone
        Caption = #1080#1079
        TabOrder = 3
      end
      object Panel5: TPanel
        Left = 97
        Top = 0
        Width = 73
        Height = 26
        Align = alLeft
        BevelOuter = bvNone
        Caption = '00:00:00'
        TabOrder = 4
        ExplicitLeft = 184
        ExplicitTop = -1
      end
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 328
    Top = 8
  end
end
