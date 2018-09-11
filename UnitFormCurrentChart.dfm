object FormCurrentChart: TFormCurrentChart
  Left = 0
  Top = 0
  Caption = 'FormCurrentChart'
  ClientHeight = 599
  ClientWidth = 1000
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
  object Panel14: TPanel
    Left = 0
    Top = 5
    Width = 5
    Height = 589
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 0
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1000
    Height = 5
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 1
  end
  object Panel4: TPanel
    Left = 995
    Top = 5
    Width = 5
    Height = 589
    Align = alRight
    BevelOuter = bvNone
    TabOrder = 2
  end
  object Panel8: TPanel
    Left = 0
    Top = 594
    Width = 1000
    Height = 5
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
  end
  object Panel2: TPanel
    Left = 5
    Top = 5
    Width = 114
    Height = 589
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 4
    object ListBox1: TListBox
      Left = 0
      Top = 37
      Width = 114
      Height = 552
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
    end
    object Panel11: TPanel
      Left = 0
      Top = 0
      Width = 114
      Height = 32
      Align = alTop
      Alignment = taLeftJustify
      BevelOuter = bvNone
      Caption = '   '#1055#1072#1088#1072#1084#1077#1090#1088
      Color = clGradientInactiveCaption
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentBackground = False
      ParentFont = False
      TabOrder = 1
    end
    object Panel3: TPanel
      Left = 0
      Top = 32
      Width = 114
      Height = 5
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 2
    end
  end
  object Panel5: TPanel
    Left = 124
    Top = 5
    Width = 111
    Height = 589
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 5
    object Panel6: TPanel
      Left = 0
      Top = 0
      Width = 111
      Height = 32
      Align = alTop
      Alignment = taLeftJustify
      BevelOuter = bvNone
      Caption = '   '#1055#1088#1080#1073#1086#1088#1099
      Color = clGradientInactiveCaption
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentBackground = False
      ParentFont = False
      TabOrder = 0
    end
    object Panel7: TPanel
      Left = 0
      Top = 32
      Width = 111
      Height = 5
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 1
    end
    object ListBox2: TListBox
      Left = 0
      Top = 37
      Width = 111
      Height = 552
      Align = alClient
      ItemHeight = 13
      TabOrder = 2
    end
  end
  object Panel9: TPanel
    Left = 119
    Top = 5
    Width = 5
    Height = 589
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 6
  end
  object Panel10: TPanel
    Left = 235
    Top = 5
    Width = 5
    Height = 589
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 7
  end
  object Chart1: TChart
    Left = 240
    Top = 5
    Width = 755
    Height = 589
    Legend.Alignment = laBottom
    Legend.Title.Visible = False
    Legend.Visible = False
    MarginLeft = 30
    MarginRight = 14
    MarginTop = 0
    MarginUnits = muPixels
    Title.Font.Charset = RUSSIAN_CHARSET
    Title.Font.Color = clBlack
    Title.Font.Height = -19
    Title.Font.Name = 'Consolas'
    Title.Text.Strings = (
      'TChart')
    Title.VertMargin = 0
    RightAxis.Visible = False
    TopAxis.Visible = False
    View3D = False
    Align = alClient
    BevelOuter = bvNone
    Color = clWindow
    TabOrder = 8
    DefaultCanvas = 'TGDIPlusCanvas'
    ColorPaletteIndex = 13
  end
end
