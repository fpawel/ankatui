unit UnitFormCurrentChart;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VclTee.TeeGDIPlus, VclTee.TeEngine,
    VclTee.TeeProcs, VclTee.Chart, Vcl.StdCtrls, Vcl.ExtCtrls,
    System.Generics.collections, System.Generics.Defaults, models, VclTee.Series, Vcl.ComCtrls,
    Vcl.ToolWin, System.ImageList, Vcl.ImgList;

type
    ProductVar = record
        ProductSerial, VarID: integer;
    end;

    TFormCurrentChart = class(TForm)
        Panel14: TPanel;
        Panel1: TPanel;
        Panel4: TPanel;
        Panel8: TPanel;
        Panel10: TPanel;
        ImageList1: TImageList;
        ListBox1: TListBox;
        PanelConsolePlaceholder: TPanel;
        Panel12: TPanel;
        ToolBar1: TToolBar;
        ToolButton1: TToolButton;
        Chart1: TChart;
        procedure FormCreate(Sender: TObject);
        procedure ListBox1Click(Sender: TObject);
        procedure ToolButton1Click(Sender: TObject);
    private
        { Private declarations }
        FSeries: TDictionary<ProductVar, TFastLineSeries>;

    public
        { Public declarations }
        procedure AddValue(product_serial, var_id: integer; value: double);
        function NewChart(title: string): string;
    end;

var
    FormCurrentChart: TFormCurrentChart;

implementation

{$R *.dfm}

uses UnitData, stringutils, dateutils, StrUtils;

function CompareVars(List: TStringList; Index1, Index2: integer): integer;
var
    d1, d2: integer;
begin
    d1 := DataModule1.GetDeviceVarByName(List[Index1]);
    d2 := DataModule1.GetDeviceVarByName(List[Index2]);

    if d1 < d2 then
        Result := -1
    else if d1 > d2 then
        Result := 1
    else
        Result := 0;
end;

procedure TFormCurrentChart.FormCreate(Sender: TObject);
begin
    FSeries := TDictionary<ProductVar, TFastLineSeries>.create;
    Chart1.title.Visible := false;
end;

procedure TFormCurrentChart.ListBox1Click(Sender: TObject);
var
    i, dev_var: integer;
    k: ProductVar;
    xs: array of TChartSeries;
begin
    SetLength(xs, 0);
    Chart1.RemoveAllSeries;
    for i := 0 to ListBox1.Items.Count - 1 do
    begin
        if not ListBox1.Selected[i] then
            Continue;
        dev_var := DataModule1.GetDeviceVarByName(ListBox1.Items[i]);
        for k in FSeries.Keys do
        begin
            if k.VarID = dev_var then
            begin
                // Chart1.AddSeries(FSeries[k]);
                FSeries[k].Tag := (k.VarID * 100000) + k.ProductSerial;
                SetLength(xs, length(xs) + 1);
                xs[length(xs) - 1] := FSeries[k];

            end;
        end;
    end;

    TArray.Sort<TChartSeries>(xs, TDelegatedComparer<TChartSeries>.Construct(
        function(const a, b: TChartSeries): integer
        begin
            Result := TComparer<integer>.Default.Compare(a.tag,
              b.tag);
        end));

    for i := 0 to length(xs) - 1 do
    begin
        Chart1.AddSeries(xs[i]);

    end;

end;

procedure TFormCurrentChart.ToolButton1Click(Sender: TObject);
begin
    Hide;
end;

function TFormCurrentChart.NewChart(title: string): string;
var
    ser: TFastLineSeries;
    k: ProductVar;
begin
    Chart1.RemoveAllSeries;
    for ser in FSeries.Values do
    begin
        ser.Free;
    end;
    FSeries.Clear;
    ListBox1.Clear;
    Panel12.Caption := format('%s %s', [datetimetostr(now), title]);
    exit('');

end;

procedure TFormCurrentChart.AddValue(product_serial, var_id: integer;
value: double);
var
    varName: string;
    ser: TFastLineSeries;
    k: ProductVar;
    sl: TStringList;
    n: integer;
begin
    varName := DataModule1.GetDeviceVarName(var_id);
    k.ProductSerial := product_serial;
    k.VarID := var_id;
    if ListBox1.Items.IndexOf(varName) = -1 then
    begin
        n := ListBox1.Items.Add(varName);
        ListBox1.Selected[n] := false;
        sl := TStringList.create;
        sl.Assign(ListBox1.Items);
        sl.CustomSort(CompareVars);
        ListBox1.Items.Assign(sl);
        sl.Free;
    end;
    if not FSeries.TryGetValue(k, ser) then
    begin
        ser := TFastLineSeries.create(nil);
        ser.XValues.DateTime := true;
        ser.title := IntToStr(product_serial) + ' ' + varName;
        FSeries.Add(k, ser);
        if ListBox1.Selected[ListBox1.Items.IndexOf(varName)] then
        begin
            Chart1.AddSeries(ser);
        end;

    end;
    ser.AddXY(now, value);
end;

end.
