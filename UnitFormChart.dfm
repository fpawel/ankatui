object FormChart: TFormChart
  Left = 0
  Top = 0
  Caption = 'FormChart'
  ClientHeight = 576
  ClientWidth = 1093
  Color = clWindow
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -15
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 18
  object Splitter1: TSplitter
    Left = 489
    Top = 0
    Width = 5
    Height = 576
    ExplicitLeft = 289
    ExplicitHeight = 504
  end
  object VirtualStringTree1: TVirtualStringTree
    Left = 0
    Top = 0
    Width = 489
    Height = 576
    Align = alLeft
    DefaultNodeHeight = 25
    Header.AutoSizeIndex = -1
    Header.Height = 23
    Header.Options = [hoColumnResize, hoDrag, hoShowSortGlyphs, hoVisible, hoFullRepaintOnResize]
    Header.ParentFont = True
    Images = ImageList1
    TabOrder = 0
    OnBeforeCellPaint = VirtualStringTree1BeforeCellPaint
    OnChange = VirtualStringTree1Change
    OnCollapsed = VirtualStringTree1Collapsed
    OnExpanding = VirtualStringTree1Expanding
    OnGetText = VirtualStringTree1GetText
    OnPaintText = VirtualStringTree1PaintText
    OnGetImageIndex = VirtualStringTree1GetImageIndex
    Columns = <
      item
        Position = 0
        Width = 204
        WideText = #1050#1072#1090#1072#1083#1086#1075
      end
      item
        Position = 3
        Width = 84
        WideText = #1042#1088#1077#1084#1103
      end
      item
        Position = 4
        Width = 70
        WideText = #1055#1072#1088#1090#1080#1103
      end
      item
        Position = 2
        WideText = #8470
      end
      item
        Position = 1
        Width = 38
      end>
  end
  object Chart1: TChart
    Left = 494
    Top = 0
    Width = 599
    Height = 576
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
    View3D = False
    Align = alClient
    BevelOuter = bvNone
    Color = clWindow
    TabOrder = 1
    DefaultCanvas = 'TGDIPlusCanvas'
    ColorPaletteIndex = 13
  end
  object ImageList1: TImageList
    ColorDepth = cd32Bit
    BlendColor = clWindow
    BkColor = clWhite
    DrawingStyle = dsTransparent
    Left = 128
    Top = 168
    Bitmap = {
      494C010106009C006C0110001000FFFFFF002110FFFFFFFFFFFFFFFF424D3600
      0000000000003600000028000000400000002000000001002000000000000020
      000000000000000000000000000000000000B5794DFFB5794DFFB5794DFFB579
      4DFFB5794DFFB5794DFFB5794DFFB5794DFFB5794DFFB5794DFFB5794DFFB579
      4DFFB5794DFFB5794DFFB5794DFFB5794DFF0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B5794DFFF0B78BFFF0B78BFFF0B7
      8BFFF0B78BFFF0B78BFFF0B78BFFF0B78BFFF0B78BFFF0B78BFFF0B78BFFF0B7
      8BFFF0B78BFFF0B78BFFF0B78BFFB5794DFF0000000000000000000000003728
      18840202011E000000003E2D1C8D000000080000000842301E91000000000202
      011E382819850000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B5794DFFF0B78BFFFAE8DBFFF0B7
      8BFFFAE8DBFFF0B78BFFFAE8DBFFF0B78BFFF0B78BFFF0B78BFFF0B78BFFF0B7
      8BFFF0B78BFFFAE8DBFFF0B78BFFB5794DFF00000000372718825A4129A3C696
      62F2956D44D35A4129A3D49E67FB6A4E30B36A4E30B3D49F67FB5A4129A3956D
      44D3C69662F25A4129A336271781000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000B5794DFFF0B78BFFF0B78BFFF0B7
      8BFFF0B78BFFF0B78BFFF0B78BFFF0B78BFFF0B78BFFF0B78BFFF0B78BFFF0B7
      8BFFF0B78BFFF0B78BFFF0B78BFFB5794DFF04020126DDAC77F6FDD7AFFFFDD2
      A4FFFDD2A4FFFDD7AFFFFDCE9CFFFDD5ABFFFDD6ABFFFDCE9CFFFDD7AFFFFDD2
      A4FFFDD2A4FFFDD7AFFFDDAC76F6040201260000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000A87148F5FEE5BDFFFFE8C2FFFFE8
      C2FFFFE8C2FFFFE8C2FFFFE8C2FFFFE8C2FFFFE8C2FFFFE8C2FFFFE8C2FFFFE8
      C2FFFFE8C2FFFFE8C2FFFEE5BDFFA87148F50604022DF3C08AFFFDD7AFFFFDD2
      A4FFFDD2A4FFFDD7AFFFFDCE9CFFFDD6ACFFFDD6ACFFFDCE9CFFFDD7AFFFFDD2
      A4FFFDD2A4FFFDD7AFFFF3C08AFF0604022D0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000865A3AD8F9DBB3FFFFE8C2FFEFD0
      A8FFC18B5FFFB98054FFDAB187FFFEE7C1FFFFE8C2FFFFE8C2FFFFE8C2FFFFE8
      C2FFFFE8C2FFFFE8C2FFF9DAB3FF835939D70604022DF3C08AFFFEE3C6FFFDCC
      98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC
      98FFFDCC98FFFEE3C6FFF3C08AFF0604022D0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000593C27B1F2CEA6FFFFE8C2FFC18B
      5FFFE6C49BFFF8DDB7FFC69367FFD8AF86FFFFE8C2FFFFE8C2FFFFE8C2FFFFE8
      C2FFFFE8C2FFFFE8C2FFF2CEA6FF583B27B00604022DF3C08AFFFDD0A0FFFDCC
      98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC
      98FFFDCC98FFFDD0A0FFF3C08AFF0604022D0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000002F1F1482E7BF96FEFFE8C2FFB981
      53FFF8DDB7FFD9B087FFE6C49CFFBA8155FFFFE8C2FFFFE8C2FFFFE8C2FFFFE8
      C2FFFFE8C2FFFFE8C2FFE6BE96FE2E1F14810604022DF3C08AFFFEDFBEFFFDCC
      98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC
      98FFFDCC98FFFEDFBEFFF3C08AFF0604022D0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000170F0A5CCEA47CF7FFE8C2FFD8AE
      84FFC69367FFE4C298FFB97E53FFBE885CFFFFE8C2FFFFE8C2FFFFE8C2FFFFE8
      C2FFFFE8C2FFFFE8C2FFCDA47CF7170F095B0604022DF3C08AFFFEDFBEFFFDCC
      98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC
      98FFFDCC98FFFEDFBEFFF3C08AFF0604022D0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000008050337BA8C64F3FFE8C2FFFEE7
      C1FFD9B087FFBC8558FFBE875BFFB97E52FFBE875BFFB98054FFDAB187FFFEE7
      C1FFFFE8C2FFFFE8C2FFBA8B63F3080503360604022DF3C08AFFFDD0A0FFFDCC
      98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC
      98FFFDCC98FFFDD0A0FFF3C08AFF0604022D0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000012B27C53F8FFE8C2FFFFE8
      C2FFFFE8C2FFFFE8C2FFFFE8C2FFBE875AFFB97E53FFE7C39BFFC69367FFD8AF
      86FFFFE8C2FFFFE8C2FFB27C53F8000000120604022DF3C08AFFFEE3C6FFFDCC
      98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC98FFFDCC
      98FFFDCC98FFFEE3C6FFF3C08AFF0604022D0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000A87148F5FEE4BEFFFFE8
      C2FFFFE8C2FFFFE8C2FFFFE8C2FFB98153FFE7C49CFFD8AE84FFF7DCB6FFBA81
      55FFFFE8C2FFFDE4BEFFA87148F4000000000604022DF3C08AFFFDD7AFFFFDD2
      A4FFFDD2A4FFFDD7AFFFFDCE9CFFFDD5ABFFFDD6ABFFFDCE9CFFFDD7AFFFFDD2
      A4FFFDD2A4FFFDD7AFFFF3C08AFF0604022D0000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000865A3AD8F9DAB2FFFFE8
      C2FFFFE8C2FFFFE8C2FFFFE8C2FFD8AE84FFC69367FFF6DBB3FFE7C49CFFC18C
      61FFFFE8C2FFF9DAB2FF855A3AD80000000004030127DEAC78F7FDD7AFFFFDD2
      A4FFFDD2A4FFFDD7AFFFFDCE9CFFFDD6ACFFFDD6ACFFFDCE9CFFFDD7AFFFFDD2
      A4FFFDD2A4FFFDD7AFFFDDAC77F6040201260000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000593C27B1F2CDA5FFFFE8
      C2FFFFE8C2FFFFE8C2FFFFE8C2FFFEE7C1FFD9B087FFBC8558FFC28D61FFEFD0
      A9FFFFE8C2FFF2CEA6FF583B27B00000000000000000382818835A4129A3C696
      62F2956D44D35A4129A3D49E67FB6A4E30B36A4E30B3D49F67FB5A4129A3956D
      44D3C69662F25A4129A337271882000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000000000000000000000000000000000002F1F1482E6BE94FEFFE8
      C2FFFFE8C2FFFFE8C2FFFFE8C2FFFFE8C2FFFFE8C2FFFFE8C2FFFFE8C2FFFFE8
      C2FFFFE8C2FFE6BD93FE2E1F1481000000000000000000000000000000003728
      18840202011E000000003E2D1C8D000000080000000842301E91000000000202
      011E382919860000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000000000000000000000000000000000000160F095AB5794DFFB579
      4DFFB5794DFFB5794DFFB5794DFFB5794DFFB5794DFFB5794DFFB5794DFFB579
      4DFFB5794DFFB5794DFF150E0959000000000000000000000000000000000000
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
      000000000000000000000000000000000000271E166E5A4838A35A4838A35A48
      38A35A4838A35A4838A35A4838A35A4838A35A4838A35A4838A3120E094D0000
      000000000000000000000000000000000000120D084C39291985392919853929
      1985402E1C8E7E5C3BC47E5C3BC47E5C3BC47E5C3BC47E5C3BC47E5C3BC47E5C
      3BC47E5C3BC47E5C3BC47E5C3BC436281982060B0E4C14242C8514242C851424
      2C851629328E2E5165C42E5165C42E5165C42E5165C42E5165C42E5165C42E51
      65C42E5165C42E5165C42E5165C414232B820000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000005D4C3AA7FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF37291C830000
      0000000000000000000000000000000000005D472FA7FEDCB6FFFEDCB6FFFEDC
      B6FFF4CDA3FFF4DEC5FFFEF0DFFFFEF0DFFFFEF0DFFFFEF0DFFFFEF0DFFFFEF0
      DFFFFEF0DFFFFEF0DFFFFEF0DFFFA9855DE0213C4BA764B0DBFF64B0DBFF64B0
      DBFF5EA7CEFF79C0E4FF85CEF5FF85CEF5FF85CEF5FF85CEF5FF85CEF5FF85CE
      F5FF85CEF5FF85CEF5FF85CEF5FF44748DE07D677DDAA399B1F5A399B1F5A399
      B1F5A399B1F5A399B1F5A399B1F5A399B1F5A399B1F5A399B1F5A399B1F5A399
      B1F5A399B1F5987C96F27C5F7BE1120D11585D4C3AA7FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF91714DD25A47
      33A35A4733A35A4733A35A4733A3271E146E5D472FA7FEDCB6FFFEDCB6FFFEDC
      B6FFF4CDA3FFF4DEC5FFFEF0DFFFFEF0DFFFFEF0DFFFFEF0DFFFFEF0DFFFFEF0
      DFFFFEF0DFFFFEF0DFFFFEF0DFFFA9855DE0213C4BA764B0DBFF64B0DBFF64B0
      DBFF5EA7CEFF79C0E4FF85CEF5FF85CEF5FF85CEF5FF85CEF5FF85CEF5FF85CE
      F5FF85CEF5FF85CEF5FF85CEF5FF44748DE0A597B1F8BED8EFFFBED8EFFFBED8
      EFFFBED8EFFFB8B3CCFFBEA0B1FFC2A5B5FFBEA0B1FFB8B3CCFFBED8EFFFBED8
      EFFFBED8EFFFA99CB6F9A7A1CDFF402F3EA35D4C3AA7FFFFFFFFF4E7DAFFE9CF
      B5FFE9CFB5FFE9CFB5FFE9CFB5FFE9CFB5FFF8F1E9FFFFFFFFFFE7C7A4FFFEF0
      DFFFFEF0DFFFFEF0DFFFFEF0DFFF5D4A36A75D472FA7FEDCB6FFFEDCB6FFFEDC
      B6FFF4CDA3FFF4DEC5FFFEF0DFFFFEF0DFFFFEF0DFFFFEF0DFFFFEF0DFFFFEF0
      DFFFFEF0DFFFFEF0DFFFFEF0DFFFA9855DE0213C4BA764B0DBFF64B0DBFF64B0
      DBFF5EA7CEFF79C0E4FF85CEF5FF85CEF5FF85CEF5FF85CEF5FF85CEF5FF85CE
      F5FF85CEF5FF85CEF5FF85CEF5FF44748DE09F7D99F9B1A2BCFFB1A2BCFFB1A2
      BCFFB4A9C3FFB9AAC1FFE3CBC6FFC49FABFFE3CBC6FFB9AAC1FFB4A9C3FFB1A2
      BCFFB1A2BCFFA27E9DFAA7A1CDFF402F3EA35D4C3AA7FFFFFFFFF4E7D9FFE9CF
      B5FFE9CFB5FFE9CFB5FFE9CFB5FFFCF9F6FFFFFFFFFFFFFFFFFFE1BC94FFF3DB
      C0FFF3DBC0FFF9E5D0FFFEF0DFFF5D4A36A75D472FA7FEDCB6FFFEDCB6FFFEDC
      B6FFF4CDA3FFF4DEC5FFFEF0DFFFFEF0DFFFFEF0DFFFFEF0DFFFFEF0DFFFFEF0
      DFFFFEF0DFFFFEF0DFFFFEF0DFFFA9855DE0213C4BA764B0DBFF64B0DBFF64B0
      DBFF5EA7CEFF79C0E4FF85CEF5FF85CEF5FF85CEF5FF85CEF5FF85CEF5FF85CE
      F5FF85CEF5FF85CEF5FF85CEF5FF44748DE0927E93E3DEEFFAFFDEEFFAFFDEEF
      FAFFDEEFFAFFD3D5E4FFCFB1B8FFEDD8CEFFCEB0B8FFD2D4E4FFDEEFFAFFDEEF
      FAFFDEEFFAFF948398E7A7A1CDFF402F3EA35D4C3AA7FFFFFFFFF4E7DAFFE9CF
      B5FFE9CFB5FFE9CFB5FFE9CFB5FFE9CFB5FFF8F1E9FFFFFFFFFFE1BC94FFF3DB
      C0FFF3DBC0FFF9E5D0FFFEF0DFFF5D4A36A75D472FA7FEDCB6FFFEDCB6FFFEDC
      B6FFF4CDA3FFF4DEC5FFFEF0DFFFFEF0DFFFFEF0DFFFFEF0DFFFFEF0DFFFFEF0
      DFFFFEF0DFFFFEF0DFFFFEF0DFFFA9855DE0213C4BA764B0DBFF64B0DBFF64B0
      DBFF5EA7CEFF79C0E4FF85CEF5FF85CEF5FF85CEF5FF85CEF5FF85CEF5FF85CE
      F5FF85CEF5FF85CEF5FF85CEF5FF44748DE0927E93E3DEEFFAFFDEEFFAFFDEEF
      FAFFDEEFFAFFDEEFFAFFAF93ACFFB7A4B8FFAF93ACFFDEEFFAFFDEEFFAFFDEEF
      FAFFDEEFFAFF948398E7A7A1CDFF402F3EA35D4C3AA7FFFFFFFFFAF3EDFFF4E7
      DAFFF4E7DAFFF4E7DAFFF4E7DAFFFEFCFBFFFFFFFFFFFFFFFFFFDBB183FFE8C6
      A2FFF7E4CDFFFEF0DFFFFEF0DFFF5D4A36A75D472FA7FEDCB6FFFEDCB6FFFEDC
      B6FFF4CDA3FFF4DEC5FFFEF0DFFFFEF0DFFFFEF0DFFFFEF0DFFFFEF0DFFFFEF0
      DFFFFEF0DFFFFEF0DFFFFEF0DFFFA9855DE0213C4BA764B0DBFF64B0DBFF64B0
      DBFF5EA7CEFF79C0E4FF85CEF5FF85CEF5FF85CEF5FF85CEF5FF85CEF5FF85CE
      F5FF85CEF5FF85CEF5FF85CEF5FF44748DE0927E93E3DEEFFAFFDEEFFAFFDEEF
      FAFFDEEFFAFFDEEFFAFFB2A7C0FFA8B6D1FFB0A6C1FFDEEFFAFFDEEFFAFFDEEF
      FAFFDEEFFAFF948398E7A7A1CDFF402F3EA35D4C3AA7FFFFFFFFFAF3EDFFF4E7
      DAFFF4E7DAFFF4E7DAFFF4E7DAFFFEFCFBFFFFFFFFFFFFFFFFFFDBB183FFE8C6
      A2FFE8C6A2FFF3DBC0FFFEF0DFFF5D4A36A75D472FA7FEDCB6FFFEDCB6FFFEDC
      B6FFF4CDA3FFF4DEC5FFFEF0DFFFFEF0DFFFFDEFDDFFE7C29BFFE8C198FFE8C1
      98FFE8C198FFE8C198FFE8C198FF9A744ED7213C4BA764B0DBFF64B0DBFF64B0
      DBFF5EA7CEFF79C0E4FF85CEF5FF85CEF5FF84CDF4FF64A7CAFF62A5C9FF62A5
      C9FF62A5C9FF62A5C9FF62A5C9FF3B657DD7927E93E3DEEFFAFFDEEFFAFFDEEF
      FAFFDEEFFAFFDEEFFAFFB2A7C0FFA8B4CEFFB0A6C1FFDEEFFAFFDEEFFAFFDEEF
      FAFFDEEFFAFF948398E7A7A1CDFF402F3EA35D4C3AA7FFFFFFFFF4E7DAFFE9CF
      B5FFE9CFB5FFE9CFB5FFE9CFB5FFE9CFB5FFF8F1E9FFFFFFFFFFDBB183FFE8C6
      A2FFF7E4CDFFFEF0DFFFFEF0DFFF5D4A36A75D472FA7FEDCB6FFFEDCB6FFFEDC
      B6FFF4CDA3FFE3BA8EFFE8C49CFFE8C49CFFE2B88BFFF9D5ADFFFEDCB6FFFEDC
      B6FFFEDCB6FFFEDCB6FFFEDCB6FF5D472FA7213C4BA764B0DBFF64B0DBFF64B0
      DBFF5EA7CEFF5FA1C3FF66A9CCFF66A9CCFF5D9EC0FF63ACD5FF64B0DBFF64B0
      DBFF64B0DBFF64B0DBFF64B0DBFF213C4BA7927E93E3DEEFFAFFDEEFFAFFDEEF
      FAFFDEEFFAFFDEEFFAFFB2A7C0FFABCCE5FFB0A6C1FFDEEFFAFFDEEFFAFFDEEF
      FAFFDEEFFAFF948398E7A7A1CDFF402F3EA35D4C3AA7FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFDBB183FFE8C6
      A2FFE8C6A2FFF3DBC0FFFEF0DFFF5D4A36A75D472FA7FEDCB6FFFEDCB6FFFEDC
      B6FFFEDCB6FFFEDCB6FFFEDCB6FFFEDCB6FFFEDCB6FFFEDCB6FFFEDCB6FFFEDC
      B6FFFEDCB6FFFEDCB6FFFEDCB6FF5D472FA7213C4BA764B0DBFF64B0DBFF64B0
      DBFF64B0DBFF64B0DBFF64B0DBFF64B0DBFF64B0DBFF64B0DBFF64B0DBFF64B0
      DBFF64B0DBFF64B0DBFF64B0DBFF213C4BA7927E93E3DEEFFAFFDEEFFAFFDEEF
      FAFFDEEFFAFFDEEFFAFFB2A7C0FFABCCE5FFB0A6C1FFDEEFFAFFDEEFFAFFDEEF
      FAFFDEEFFAFF948398E7A79FCBFF3D2C3A9E5D4C3AA7FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFF5EADFFFE3BD95FFE8C8A5FFD8AA78FFE6C5A0FFFEF0
      DFFFFEF0DFFFFEF0DFFFFEF0DFFF5D4A36A75D472FA7FEDCB6FFFEDCB6FFFEDC
      B6FFFEDCB6FFFEDCB6FFFEDCB6FFFEDCB6FFFEDCB6FFFEDCB6FFFEDCB6FFFEDC
      B6FFFEDCB6FFFEDCB6FFFEDCB6FF5D472FA7213C4BA764B0DBFF64B0DBFF64B0
      DBFF64B0DBFF64B0DBFF64B0DBFF64B0DBFF64B0DBFF64B0DBFF64B0DBFF64B0
      DBFF64B0DBFF64B0DBFF64B0DBFF213C4BA7927E93E3DEEFFAFFDEEFFAFFDEEF
      FAFFDEEFFAFFDEEFFAFFC0D1E3FFA9B9D2FFB0A6C1FFDEEFFAFFDEEFFAFFDEEF
      FAFFDEEFFAFF877689DB382A3697020102205D4C3AA7FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFF5EADFFFF4DEC5FFEED3B5FFE3C098FFFEF0DFFFFEF0
      DFFFFEF0DFFFFEF0DFFFFEF0DFFF5D4A36A75D472FA7FEDCB6FFFEDCB6FFFEDC
      B6FFFEDCB6FFFBD8B1FFF3CBA0FFF3CBA0FFF3CBA0FFF3CBA0FFF3CBA0FFF3CB
      A0FFF3CBA0FFF3CBA0FFF3CBA0FF624A31AC213C4BA764B0DBFF64B0DBFF64B0
      DBFF64B0DBFF62ADD8FF5EA5CDFF5EA5CDFF5EA5CDFF5EA5CDFF5EA5CDFF5EA5
      CDFF5EA5CDFF5EA5CDFF5EA5CDFF233F4EACB8A0B7F8F0F7FFFFF0F7FFFFF0F7
      FFFFF0F7FFFFE1DEEBFF7D677AD6564C59B64B3E4CA93F323D9B3F323D9B3F32
      3D9B3F323D9B1913176400000000000000005D4C3AA7FFFFFFFFFFFFFFFFFFFF
      FFFFFFFFFFFFFFFFFFFFF5EADFFFE4C19AFFE3C098FFFEF0DFFFFEF0DFFFFEF0
      DFFFDBA872FFE8BC8FFFDCAA78FE745535BE5D472FA7FEDCB6FFFEDCB6FFFEDC
      B6FFF7D3ACFE694F33B207050233070502330705023307050233070502330705
      02330705023307050233070502330201001E213C4BA764B0DBFF64B0DBFF64B0
      DBFF62A9D3FE254252B202040533020405330204053302040533020405330204
      05330204053302040533020405330001021E866C80DDB59FB2F7B59FB2F7B59F
      B2F7B49EB1F7655362BD00000001000000000000000000000000000000000000
      000000000000000000000000000000000000271E166E5A4838A35A4838A35A48
      38A35A4838A3957859D3E4C3A0FFE4C099FFFEF0DFFFFEF0DFFFFEF0DFFFFEF0
      DFFFE7BB8CFFF4CEA4FF896843CD00000000120D084C39291985392919853929
      19853022157A0000000600000000000000000000000000000000000000000000
      000000000000000000000000000000000000060B0E4C14242C8514242C851424
      2C85111E257A0000000600000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      00000000000037291A83FEF0DFFFFEF0DFFFFEF0DFFFFEF0DFFFFEF0DFFFFEF0
      DFFFDBAA77FE896843CD00000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000120E094D5A4733A35A4733A35A4733A35A4733A35A4733A35A47
      33A3725334BC000000000000000000000000424D3E000000000000003E000000
      2800000040000000200000000100010000000000000100000000000000000000
      000000000000000000000000FFFFFF0000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      0000000000000000000000000000000000000000000000000000000000000000
      000000000000}
  end
end
