object FormDelay: TFormDelay
  Left = 0
  Top = 0
  Caption = 'FormDelay'
  ClientHeight = 32
  ClientWidth = 1016
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 18
  object Panel1: TPanel
    Left = 393
    Top = 0
    Width = 623
    Height = 32
    Align = alClient
    Alignment = taLeftJustify
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitLeft = 263
    ExplicitWidth = 759
    ExplicitHeight = 31
    object LabelCurrentTime: TLabel
      Left = 74
      Top = 0
      Width = 58
      Height = 18
      Align = alLeft
      Caption = '00:00:00'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clTeal
      Font.Height = -15
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
    end
    object LabelTotalTime: TLabel
      Left = 0
      Top = 0
      Width = 58
      Height = 18
      Align = alLeft
      Caption = '00:00:00'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -15
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
    end
    object LabelWhat: TLabel
      Left = 195
      Top = 0
      Width = 23
      Height = 18
      Align = alLeft
      Caption = '?dd'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clNavy
      Font.Height = -15
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
    end
    object LabelProgress: TLabel
      Left = 148
      Top = 0
      Width = 31
      Height = 18
      Align = alLeft
      Caption = '50%'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clTeal
      Font.Height = -15
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Layout = tlCenter
    end
    object Panel2: TPanel
      Left = 58
      Top = 0
      Width = 16
      Height = 32
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitLeft = 97
      ExplicitHeight = 31
    end
    object Panel5: TPanel
      Left = 179
      Top = 0
      Width = 16
      Height = 32
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitLeft = 195
      ExplicitHeight = 31
    end
    object Panel3: TPanel
      Left = 132
      Top = 0
      Width = 16
      Height = 32
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 2
      ExplicitLeft = 170
      ExplicitHeight = 31
    end
  end
  object ToolBar1: TToolBar
    Left = 0
    Top = 0
    Width = 30
    Align = alLeft
    ButtonHeight = 30
    ButtonWidth = 30
    Caption = 'ToolBarCancel'
    EdgeInner = esNone
    EdgeOuter = esNone
    Images = ImageList4
    TabOrder = 1
    ExplicitLeft = 629
    ExplicitHeight = 31
    object ToolButtonStop: TToolButton
      Left = 0
      Top = 0
      Hint = #1055#1088#1077#1088#1074#1072#1090#1100' '#1074#1099#1087#1086#1083#1085#1077#1085#1080#1077
      Caption = 'ToolButtonCancel'
      ImageIndex = 0
      ParentShowHint = False
      ShowHint = True
      OnClick = ToolButton1Click
    end
  end
  object Panel4: TPanel
    Left = 30
    Top = 0
    Width = 363
    Height = 32
    Align = alLeft
    BevelOuter = bvNone
    TabOrder = 2
    DesignSize = (
      363
      32)
    object ProgressBar1: TProgressBar
      Left = 6
      Top = 8
      Width = 351
      Height = 17
      Anchors = [akLeft, akTop, akRight, akBottom]
      TabOrder = 0
    end
  end
  object Timer1: TTimer
    OnTimer = Timer1Timer
    Left = 328
    Top = 8
  end
  object ImageList4: TImageList
    ColorDepth = cd32Bit
    BlendColor = clWindow
    BkColor = clWhite
    DrawingStyle = dsTransparent
    Height = 20
    Width = 20
    Left = 576
    Top = 3
    Bitmap = {
      494C0101010040034C0314001400FFFFFF002110FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000500000001400000001002000000000000019
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000120D074C594025A85940
      25A8594025A8594025A8594025A8594025A8594025A8594025A8594025A85940
      25A8594025A8594025A8594025A8594025A8594025A8594025A8150F08530000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004A341E98FDCC98FFFDCC
      98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC
      98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FF553C23A30000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004A341E98FDCC98FFFDCC
      98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC
      98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FF553C23A30000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004A341E98FDCC98FFFDCC
      98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC
      98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FF553C23A30000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004A341E98FDCC98FFFDCC
      98FFFDCC98FFFDCC98FFFEDEBDFFFDD4A9FFFDCC98FFFDCC98FFFDCC98FFFEE4
      C8FFFFFFFFFFFEE7CFFFFDCC98FFFDCC98FFFDCC98FFFDCC98FF553C23A30000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004A341E98FDCC98FFFDCC
      98FFFDCC98FFFDCC98FFFEE4C8FFFFFBF8FFFDD7AFFFFDCC98FFFDCC98FFFEE4
      C8FFFFFFFFFFFEE7CFFFFDCC98FFFDCC98FFFDCC98FFFDCC98FF553C23A30000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004A341E98FDCC98FFFDCC
      98FFFDCC98FFFDCC98FFFEE4C8FFFFFFFFFFFFFDFBFFFEDBB6FFFDCC98FFFEE4
      C8FFFFFFFFFFFEE7CFFFFDCC98FFFDCC98FFFDCC98FFFDCC98FF553C23A30000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004A341E98FDCC98FFFDCC
      98FFFDCC98FFFDCC98FFFEE4C8FFFFFFFFFFFFFFFFFFFFFEFDFFFEDFBEFFFEE4
      C8FFFFFFFFFFFEE7CFFFFDCC98FFFDCC98FFFDCC98FFFDCC98FF553C23A30000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004A341E98FDCC98FFFDCC
      98FFFDCC98FFFDCC98FFFEE4C8FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEF0
      E1FFFFFFFFFFFEE7CFFFFDCC98FFFDCC98FFFDCC98FFFDCC98FF553C23A30000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004A341E98FDCC98FFFDCC
      98FFFDCC98FFFDCC98FFFEE4C8FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFEF0
      E1FFFFFFFFFFFEE7CFFFFDCC98FFFDCC98FFFDCC98FFFDCC98FF553C23A30000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004A341E98FDCC98FFFDCC
      98FFFDCC98FFFDCC98FFFEE4C8FFFFFFFFFFFFFFFFFFFFFEFDFFFEDFBEFFFEE4
      C8FFFFFFFFFFFEE7CFFFFDCC98FFFDCC98FFFDCC98FFFDCC98FF553C23A30000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004A341E98FDCC98FFFDCC
      98FFFDCC98FFFDCC98FFFEE4C8FFFFFFFFFFFFFDFBFFFEDBB6FFFDCC98FFFEE4
      C8FFFFFFFFFFFEE7CFFFFDCC98FFFDCC98FFFDCC98FFFDCC98FF553C23A30000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004A341E98FDCC98FFFDCC
      98FFFDCC98FFFDCC98FFFEE4C8FFFFFBF8FFFDD7AFFFFDCC98FFFDCC98FFFEE4
      C8FFFFFFFFFFFEE7CFFFFDCC98FFFDCC98FFFDCC98FFFDCC98FF553C23A30000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004A341E98FDCC98FFFDCC
      98FFFDCC98FFFDCC98FFFEDEBDFFFDD4A9FFFDCC98FFFDCC98FFFDCC98FFFEE4
      C8FFFFFFFFFFFEE7CFFFFDCC98FFFDCC98FFFDCC98FFFDCC98FF553C23A30000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004A341E98FDCC98FFFDCC
      98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC
      98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FF553C23A30000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004A341E98FDCC98FFFDCC
      98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC
      98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FF553C23A30000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000004A341E98FDCC98FFFDCC
      98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC
      98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FF553C23A30000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000E0A06444E37219B4E37
      219B4E37219B4E37219B4E37219B4E37219B4E37219B4E37219B4E37219B4E37
      219B4E37219B4E37219B4E37219B4E37219B4E37219B4E37219B110C074B0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000050000000140000000100010000000000F00000000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000}
  end
end
