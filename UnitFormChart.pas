unit UnitFormChart;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, FireDAC.Comp.Client, UnitData,
    VirtualTrees, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.ComCtrls, HTMLUn2, HtmlView,
    System.ImageList, UnitFormChartNodes, Vcl.ImgList, VclTee.TeeGDIPlus,
    VclTee.TeEngine, VclTee.TeeProcs, VclTee.Chart, VclTee.Series;

type

    TFormChart = class(TForm)
        ImageList1: TImageList;
        Splitter1: TSplitter;
        VirtualStringTree1: TVirtualStringTree;
        Chart1: TChart;
        procedure FormCreate(Sender: TObject);
        procedure VirtualStringTree1BeforeCellPaint(Sender: TBaseVirtualTree;
          TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
          CellPaintMode: TVTCellPaintMode; CellRect: TRect;
          var ContentRect: TRect);
        procedure VirtualStringTree1Change(Sender: TBaseVirtualTree;
          Node: PVirtualNode);
        procedure VirtualStringTree1Collapsed(Sender: TBaseVirtualTree;
          Node: PVirtualNode);
        procedure VirtualStringTree1Expanding(Sender: TBaseVirtualTree;
          Node: PVirtualNode; var Allowed: boolean);
        procedure VirtualStringTree1GetImageIndex(Sender: TBaseVirtualTree;
          Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
          var Ghosted: boolean; var ImageIndex: TImageIndex);
        procedure VirtualStringTree1GetText(Sender: TBaseVirtualTree;
          Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
          var CellText: string);
        procedure VirtualStringTree1PaintText(Sender: TBaseVirtualTree;
          const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
          TextType: TVSTTextType);
        procedure RichEdit1ContextPopup(Sender: TObject; MousePos: TPoint;
          var Handled: boolean);

    private
        { Private declarations }
    public
        { Public declarations }
    end;

var
    FormChart: TFormChart;

implementation

{$R *.dfm}

uses dateutils, FireDAC.Stan.PAram, stringutils, richeditutils,
    variantutils, stringgridutils;

function inttostr2(n: integer): string;
begin
    result := inttostr(n);
    if n < 10 then
        result := '0' + result;
end;

procedure GetSeriesValues(ser: TFastLineSeries; SeriesID: int64; AVar: integer;
  product_serial: integer);
var
    FmtStngs: TFormatSettings;
    X: string;
    y: double;
begin
    FmtStngs := TFormatSettings.Create(GetThreadLocale);
    FmtStngs.DateSeparator := '.';
    FmtStngs.ShortDateFormat := 'dd/MM/yyyy';
    FmtStngs.TimeSeparator := ':';
    FmtStngs.LongTimeFormat := 'h:mm:ss';
    with TFDQuery.Create(nil) do
    begin
        Connection := DataModule1.FDConnectionProductsDB;
        SQL.Text :=
          'SELECT x,y FROM chart_value_info WHERE product_serial = :product_serial '
          + 'AND var = :var AND series_id = :series_id;';
        ParamByName('product_serial').Value := product_serial;
        ParamByName('var').Value := AVar;
        ParamByName('series_id').Value := SeriesID;
        open;
        First;
        while not Eof do
        begin
            X := FieldValues['x'];
            y := FieldValues['y'];
            ser.AddNullXY(StrToDateTime(X, FmtStngs), y);
            Next;
        end;
        Close;
        Free;
    end;
end;

procedure freeNodeData(t: TVirtualStringTree; n: PVirtualNode);
var
    p: PTreeData;
begin
    if not Assigned(n) then
        exit;
    p := t.GetNodeData(n);
    p.X.Free;
    freeNodeData(t, n.FirstChild);
    freeNodeData(t, n.NextSibling);
end;

procedure TFormChart.FormCreate(Sender: TObject);
var
    year: integer;

begin
    VirtualStringTree1.NodeDataSize := SizeOf(RTreeData);
    for year in DataModule1.SeriesYears do
        TNodeYear.Create(VirtualStringTree1, year);
    Chart1.Visible := false;
end;

procedure TFormChart.RichEdit1ContextPopup(Sender: TObject; MousePos: TPoint;
  var Handled: boolean);
begin
    RichEdit_PopupMenu(TRichEdit(Sender));
    Handled := true;
end;

procedure TFormChart.VirtualStringTree1BeforeCellPaint(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
begin
    if Sender.Selected[Node] then
    begin
        TargetCanvas.Brush.Color := clSkyBlue;
        TargetCanvas.FillRect(CellRect);
    end;
end;

procedure TFormChart.VirtualStringTree1Change(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
    p: PTreeData;

    ser: TFastLineSeries;
    product_serial: integer;

begin

    Chart1.Visible := false;
    if not Assigned(Node) then
        exit;
    p := Sender.GetNodeData(Node);
    if p.X is TNodeVar then
        with p.X as TNodeVar do
        begin
            while Chart1.SeriesCount > 0 do
                with Chart1.Series[0] do
                begin
                    ParentChart := nil;
                    Free;
                end;

            Chart1.Title.Caption := FVarName;
            for product_serial in DataModule1.GetSeriesVarProducts
              (FSeriesInfo.SeriesID, FVar) do
            begin
                ser := TFastLineSeries.Create(nil);
                ser.XValues.DateTime := true;
                ser.Title := inttostr(product_serial);
                GetSeriesValues(ser, FSeriesInfo.SeriesID, FVar,
                  product_serial);
                Chart1.AddSeries(ser);
            end;

            with Chart1 do
            begin
                Title.Caption := FVarName;
                Visible := true;
            end;

        end
    else if p.X is TNodeVarProduct then
        with p.X as TNodeVarProduct do
        begin
            while Chart1.SeriesCount > 0 do
                with Chart1.Series[0] do
                begin
                    ParentChart := nil;
                    Free;
                end;

            ser := TFastLineSeries.Create(nil);
            ser.XValues.DateTime := true;
            ser.Title := inttostr(FSerial);
            GetSeriesValues(ser, FSeriesInfo.SeriesID, FVar, FSerial);

            with Chart1 do
            begin
                Title.Caption := FVarName + ': ' + inttostr(FSerial);
                AddSeries(ser);
                Visible := true;
            end;

        end;

    with Chart1 do
    begin
        Legend.Visible := Visible AND (SeriesCount > 1);
        if SeriesCount = 1 then
            Title.Caption := Title.Caption + ': ' + Series[0].Title;

    end;

end;

procedure TFormChart.VirtualStringTree1Collapsed(Sender: TBaseVirtualTree;
  Node: PVirtualNode);
var
    p: PTreeData;
begin
    p := Sender.GetNodeData(Node);
    freeNodeData(VirtualStringTree1, Node.FirstChild);
    VirtualStringTree1.DeleteChildren(Node);
    p.X.FPopulated := false;
end;

procedure TFormChart.VirtualStringTree1Expanding(Sender: TBaseVirtualTree;
  Node: PVirtualNode; var Allowed: boolean);
var
    p: PTreeData;
begin
    p := Sender.GetNodeData(Node);
    if p.X.FPopulated then
        exit;
    p.X.Populate;
    p.X.FPopulated := node.ChildCount > 0;
end;

procedure TFormChart.VirtualStringTree1GetImageIndex(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex;
  var Ghosted: boolean; var ImageIndex: TImageIndex);
var
    p: PTreeData;
begin
    p := Sender.GetNodeData(Node);
    if Column in [0, 1, 2] then
        case Kind of
            ikNormal, ikSelected:
                ImageIndex := p.X.FColumn[Column].ImageIndex;

        end;

end;

procedure TFormChart.VirtualStringTree1GetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: string);
var
    p: PTreeData;
begin
    p := Sender.GetNodeData(Node);
    if Column in [0, 1, 2, 3] then
        CellText := p.X.FColumn[Column].Text;
end;

procedure TFormChart.VirtualStringTree1PaintText(Sender: TBaseVirtualTree;
  const TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType);
var
    p: PTreeData;
begin
    p := Sender.GetNodeData(Node);
    if not Column in [0, 1, 2] then
        exit;

    case Column of
        0:
            begin

                if p.X is TNodeSeries then
                begin
                    if not Sender.Selected[Node] then
                        TargetCanvas.Font.Color := clNavy;

                end
                else if p.X is TNodeVar then
                begin
                    if not Sender.Selected[Node] then
                        TargetCanvas.Font.Color := clMaroon;
                end;

            end;

    end;

end;

end.
