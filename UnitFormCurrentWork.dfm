object FormCurrentWork: TFormCurrentWork
  Left = 0
  Top = 0
  Caption = 'FormCurrentWork'
  ClientHeight = 484
  ClientWidth = 778
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -16
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 19
  object VirtualStringTree1: TVirtualStringTree
    Left = 0
    Top = 41
    Width = 778
    Height = 443
    Align = alClient
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
    Images = ImageList2
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
        Width = 429
        WideText = #1044#1077#1081#1089#1090#1074#1080#1077
      end
      item
        Position = 0
        WideText = #8470
      end>
  end
  object Button1: TButton
    Left = 0
    Top = 0
    Width = 778
    Height = 41
    Align = alTop
    Caption = '   '#1047#1072#1087#1091#1089#1090#1080#1090#1100': [0] '#1053#1072#1089#1090#1088#1086#1081#1082#1072' '#1040#1085#1082#1072#1090
    TabOrder = 1
    OnClick = Button1Click
  end
  object ImageList2: TImageList
    ColorDepth = cd32Bit
    BlendColor = clWindow
    BkColor = clWhite
    DrawingStyle = dsTransparent
    Left = 472
    Top = 256
    Bitmap = {
      494C010103001801BC0110001000FFFFFF002110FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000001000000001002000000000000010
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004C3C2C96816A
      51C2816A51C2816A51C2816A51C2816A51C2816A51C2816A51C2816A51C2816A
      51C2816A51C24C3C2C9600000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000001021A172C3280192F3685192F
      3685192F3685192F3685192F3685192F3685192F3685192F3685192F3685192F
      3685192F3685192F3685172C3280000101190000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000080694FC1FEF0
      DFFFFEF0DFFFFEF0DFFFFEF0DFFFFEF0DFFFFEF0DFFFFEF0DFFFFEF0DFFFFEF0
      DFFFFEF0DFFF80694FC100000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000025444D9DA2EDFEFFA3EEFFFFA3EE
      FFFFA3EEFFFFA3EEFFFFA3EEFFFFA3EEFFFFA3EEFFFFA3EEFFFFA3EEFFFFA3EE
      FFFFA3EEFFFFA3EEFFFFA2EDFEFF25434D9C0000000000000000000000000000
      000003020122523D289F00000003000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000080694FC1FEF0
      DFFFFEF0DFFFFEF0DFFFFEF0DFFFFEF0DFFFFEF0DFFFFEF0DFFFFEF0DFFFFEF0
      DFFFFEF0DFFF80694FC100000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000030607316EB1C3EAA3EEFFFFA3EE
      FFFFA3EEFFFFA3EEFFFFA3EEFFFF97D7E4FF97D7E4FFA3EEFFFFA3EEFFFFA3EE
      FFFFA3EEFFFFA3EEFFFF6EB1C3EA030607310000000000000000000000000302
      0122977C5DD1F6E4CFFE644D35AD000000030000000000000000000000000000
      000000000000000000000000000000000000000000000000000080694FC1FEF0
      DFFFFEF0DFFFFEF0DFFFFAE9D4FFFEF0DFFFFEF0DFFFFEF0DFFFFEF0DFFFFEF0
      DFFFFEF0DFFF80694FC100000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000264651A0A1EBFDFFA3EE
      FFFFA3EEFFFFA3EEFFFFA3EEFFFF759396FF769497FFA3EEFFFFA3EEFFFFA3EE
      FFFFA3EEFFFFA1EBFDFF264651A000000000000000000000000003020122977C
      5DD1FCECD9FFC9AF90EDF6E4D0FE644D36AE0000000300000000000000000000
      000000000000000000000000000000000000000000000000000080694FC1FEF0
      DFFFFEF0DFFFFEF0DFFFE2B688FFEECFADFFFEF0DFFFFEF0DFFFFEF0DFFFFEF0
      DFFFFEF0DFFF80694FC100000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000125790A3DAA3EE
      FFFFA3EEFFFFA3EEFFFFA3EEFFFFA0E8F8FFA0E8F8FFA3EEFFFFA3EEFFFFA3EE
      FFFFA3EEFFFF5790A3DA00000012000000000000000003020122977C5DD1FCEC
      D9FF7B6245C0100C0748AA8F74DBF6E4D0FE644D36AE00000003000000000000
      00000000000000000000000000000000000000000000000000006B5D4EABFEF0
      DFFFFEF0DFFFFEF0DFFFE7BC8FFFEFBC88FFEECEACFFFEF0DFFFFEF0DFFFFEF0
      DFFFFEF0DFFF80694FC100000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000162A307D99E2
      F4FDA3EEFFFFA3EEFFFFA3EEFFFF759295FF759295FFA3EEFFFFA3EEFFFFA3EE
      FFFF99E2F4FD162A307D000000000000000003020121977C5DD1FCECD9FF7C62
      47C00000000C000000000B08053DAA8F74DBF6E4D0FE644D36AE000000030000
      00000000000000000000000000000000000007050233070502336A5D4FABF3DB
      C0FFF3DBC0FFF3DBC0FFE2B585FFFDCC98FFEFBD88FFEECEACFFFEF0DFFFFEF0
      DFFFFEF0DFFF80694FC100000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000044375
      84C8A3EEFFFFA3EEFFFFA3EEFFFF759295FF759295FFA3EEFFFFA3EEFFFFA3EE
      FFFF437584C8000000040000000000000000120E094DBAA183E57C6247C00000
      000C0000000000000000000000000B08053DAA8F74DBF6E4D0FE644D36AE0000
      000300000000000000000000000000000000BA8A57EAF2BE88FFF2BE88FFF2BE
      88FFF2BE88FFF2BE88FFF4C08BFFFDCC98FFFDCC98FFEFBD88FFEDCEABFFFEF0
      DFFFFEF0DFFF80694FC100000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000811
      135184CADCF4A3EEFFFFA3EEFFFF759295FF759295FFA3EEFFFFA3EEFFFF84CA
      DCF408111351000000000000000000000000000000000E0A06440000000C0000
      0000000000000000000000000000000000000B08053DAA8F74DBF6E4D0FE644D
      36AE00000003000000000000000000000000BA8A57EAF2BE88FFF2BE88FFF2BE
      88FFF2BE88FFF2BE88FFF4C08BFFFDCC98FFFDCC98FFEFBD88FFEDCEABFFFEF0
      DFFFFEF0DFFF80694FC100000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000335A68B4A2EEFFFFA3EEFFFF759295FF759295FFA3EEFFFFA2EEFFFF335A
      68B4000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000B08053DAA8F74DBF7E5
      D1FE644D36AE00000003000000000000000007050233070502336A5D4FABF3DB
      C0FFF3DBC0FFF3DBC0FFE2B585FFFDCC98FFEFBD88FFEECEACFFFEF0DFFFFEF0
      DFFFFEF0DFFF80694FC100000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000103042568A8BAE6A3EEFFFF7EA4AAFF7EA4AAFFA3EEFFFF68A8BAE60103
      0425000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000000B08053DAA8F
      74DBF7E5D1FE644D36AE000000030000000000000000000000006B5D4EABFEF0
      DFFFFEF0DFFFFEF0DFFFE7BC8FFFEFBC88FFEECEACFFFEF0DFFFFEF0DFFFFEF0
      DFFFFEF0DFFF80694FC100000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000213E4797A0EBFCFFA3EEFFFFA3EEFFFFA0EBFCFF213E47970000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000B08
      053DAA8F74DBF7E5D1FE644D36AE00000003000000000000000080694FC1FEF0
      DFFFFEF0DFFFFEF0DFFFE2B688FFEECFADFFFEF0DFFFFEF0DFFFFEF0DFFFFEF0
      DFFFFEF0DFFF80694FC100000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000C51899BD6A3EEFFFFA3EEFFFF51899BD60000000C0000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000B08053DAA8F74DBE2CBB2F821180F67000000000000000080694FC1FEF0
      DFFFFEF0DFFFFEF0DFFFFAE9D4FFFEF0DFFFFEF0DFFFFEF0DFFFFEF0DFFFFEF0
      DFFFFEF0DFFF80694FC100000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000001221277193DBEEFB93DBEEFB12212771000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000B08053D4232208F00000000000000000000000080694FC1FEF0
      DFFFFEF0DFFFFEF0DFFFFEF0DFFFFEF0DFFFFEF0DFFFFEF0DFFFFEF0DFFFFEF0
      DFFFFEF0DFFF80694FC100000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000001355E6DB8355E6DB800000001000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000000000000080694FC1FEF0
      DFFFFEF0DFFFFEF0DFFFFEF0DFFFFEF0DFFFFEF0DFFFFEF0DFFFFEF0DFFFFEF0
      DFFFFEF0DFFF80694FC100000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000000000004C3C2C96816A
      51C2816A51C2816A51C2816A51C2816A51C2816A51C2816A51C2816A51C2816A
      51C2816A51C24C3C2C9600000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000424D3E000000000000003E000000
      2800000040000000100000000100010000000000800000000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000}
  end
end
