unit UnitFormCurrentChart;

interface

uses
    Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
    System.Classes, Vcl.Graphics,
    Vcl.Controls, Vcl.Forms, Vcl.Dialogs, VclTee.TeeGDIPlus, VclTee.TeEngine,
    VclTee.TeeProcs, VclTee.Chart, Vcl.StdCtrls, Vcl.ExtCtrls,
    System.Generics.collections, models, VclTee.Series;

type
    TFormCurrentChart = class(TForm)
        Panel14: TPanel;
        Panel1: TPanel;
        Panel4: TPanel;
        Panel8: TPanel;
        Panel2: TPanel;
        ListBox1: TListBox;
        Panel11: TPanel;
        Panel3: TPanel;
        Panel5: TPanel;
        Panel6: TPanel;
        Panel7: TPanel;
        Panel9: TPanel;
        ListBox2: TListBox;
        Panel10: TPanel;
        Chart1: TChart;
        procedure FormCreate(Sender: TObject);
    private
        { Private declarations }
        FSeries: TDictionary<RProductVar, TFastLineSeries>;
    public
        { Public declarations }
        procedure HandleReadVar(x: TReadVar);
    end;

var
    FormCurrentChart: TFormCurrentChart;

implementation

{$R *.dfm}

uses UnitData, stringutils;

procedure TFormCurrentChart.FormCreate(Sender: TObject);
begin
    FSeries := TDictionary<RProductVar, TFastLineSeries>.create;;
end;

procedure TFormCurrentChart.HandleReadVar(x: TReadVar);
var
    varName: string;
    ser: TFastLineSeries;
begin
//    if x.FError <> '' then
//        exit;
//    varName := stringutils.inttostr2(x.FVar) + ' ' +
//      DataModule1.GetDeviceVarName(x.FVar);
//    if ListBox1.Items.IndexOf(varName) = -1 then
//        ListBox1.Items.Add(varName);
//    if not FSeries.TryGetValue(x.ProductVar, ser) then
//    begin
//        ser := TFastLineSeries.create(nil);
//        ser.XValues.DateTime := true;
//        ser.Title := inttostr(x.FProduct);
//    end;

end;

end.
